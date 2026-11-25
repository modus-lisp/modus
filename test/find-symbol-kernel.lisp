;;;; find-symbol-kernel.lisp -- FIND-SYMBOL finds a kernel definition nothing quoted.
;;;;
;;;;   ./modus --script test/find-symbol-kernel.lisp
;;;;
;;;; The image keys its functions and globals by name, and a symbol is made only when
;;;; something reads or interns the name, so (find-symbol "W32-CALL*" "MODUS-INT") was
;;;; NIL in modus.exe until the reader had seen modus-int::w32-call*.  Every name in
;;;; the kernel's function table must now be found in MODUS-INT, as the symbol the
;;;; reader gives, and a name the kernel does not define must still be absent.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*)))

;; Take the names first, through a string, so this file's own reading interns none of them.
(defvar *names*
  (let ((h (symbol-value (find-symbol "*KERNEL-FN-NAMES*" "MODUS-INT"))) (out '()))
    (when h (maphash (lambda (k v) (declare (ignore v)) (push k out)) h))
    out))

(check "the kernel's function names are known" (> (length *names*) 1000))

(let ((missing '()))
  (dolist (n *names*)
    (unless (find-symbol n "MODUS-INT") (push n missing)))
  (check (format nil "every one of ~d kernel functions is found in MODUS-INT~@[ (missing ~{~a~^ ~})~]"
                 (length *names*) (and missing (subseq missing 0 (min 5 (length missing)))))
         (null missing)))

(let* ((n (find "%CLI-FLAG-P" *names* :test #'string=))
       (s (and n (find-symbol n "MODUS-INT"))))
  (check "the symbol found is the one the reader gives"
         (and s (eq s (read-from-string "modus-int::%cli-flag-p"))))
  (check "and names the kernel's function" (and s (fboundp s))))

(check "a name the kernel does not define is absent"
       (null (find-symbol "NO-SUCH-KERNEL-FUNCTION-QZX" "MODUS-INT")))
(check "and asking did not create it"
       (null (find-symbol "NO-SUCH-KERNEL-FUNCTION-QZX" "MODUS-INT")))
(check "a kernel name is not found in CL-USER"
       (null (find-symbol "%CLI-FLAG-P" "COMMON-LISP-USER")))

(format t "~a~%" (if (zerop *fails*) "ALL PASS" (format nil "~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
