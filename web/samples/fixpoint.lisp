;;;; fixpoint.lisp - the seed of a minimal browser fixpoint.
;;;;
;;;; A tiny trusted kernel (sha1 + this loader) that admits the rest of the
;;;; system one file at a time: a source file is compiled+loaded ONLY if its
;;;; SHA-1 matches a trusted manifest.  Here the loader measures its OWN source
;;;; -- the fixpoint seed -- then shows a matching verify and a tampered one.

(ql:quickload :sha1)                       ; the trusted hashing primitive

(defun %slurp (path)
  (with-open-file (s path :direction :input)
    (let ((str (make-string (file-length s))))
      (subseq str 0 (read-sequence str s)))))

(defvar *manifest* nil)                    ; alist (path . sha1-hex)

(defun %sha (path) (string-downcase (sha1:sha1-hex (%slurp path))))

(defun verify (path)
  "T iff PATH's sha1 matches its *manifest* entry."
  (let ((want (cdr (assoc path *manifest* :test #'string=))))
    (and want (string= (%sha path) (string-downcase want)))))

(defun verified-load (path)
  "Compile+load PATH only if it verifies against *manifest*."
  (if (verify path)
      (progn (format t "; ok      ~a~%" path) (load path) t)
      (progn (format t "; REFUSED ~a  (sha mismatch)~%" path)
             (error "verified-load: ~a failed verification" path))))

;;; --- the loader measures its own source (the fixpoint seed) -----------------
(let* ((me   "/home/web/fixpoint.lisp")
       (self (%sha me)))
  (format t "~%this loader's own SHA-1:~%  ~a~%~%" self)
  ;; trust-on-first-use: record ourselves, then verify
  (setq *manifest* (list (cons me self)))
  (format t "verify against a matching manifest:  ~a~%"
          (if (verify me) "PASS -- would admit" "FAIL"))
  ;; now flip one bit of the trusted hash
  (setq *manifest* (list (cons me "0000000000000000000000000000000000000000")))
  (format t "verify against a tampered manifest:  ~a  (refused)~%"
          (if (verify me) "PASS" "FAIL")))

(format t "~%verified-load admits a file only when its SHA-1 matches the manifest.~%")
(format t "That is the whole trick: a small trusted core hashes and loads the~%")
(format t "rest of the system, one verified file at a time.~%")
