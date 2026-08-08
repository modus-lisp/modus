(defun say (k v) (princ (concatenate 'string k "=" (princ-to-string v))) (terpri) (finish-output))
(defun try-asd (tar name)
  (say (concatenate 'string "ASD." name)
       (handler-case
           (let* ((gz (%it-slurp-bytes tar))
                  (entries (tar-extract gz))
                  (want (concatenate 'string name ".asd"))
                  (hit nil))
             (dolist (e entries)
               (when (and (null hit) (string= (%it-basename (car e)) want)) (setq hit e)))
             (if (null hit) :NO-ASD
                 (let ((forms (%it-read-asd-forms (tar-bytes-to-string (cdr hit)))))
                   (list :OK (length forms)))))
         (t (c) (list :ERR (type-of c)
                      (handler-case (%report-condition-text c) (t (c2) :NOTEXT)))))))
(dolist (n (list "ieee-floats" "iterate" "md5" "salza2" "parse-float"
                 "documentation-utils" "split-sequence" "bordeaux-threads"
                 "named-readtables" "trivial-features" "cl-ppcre" "babel" "puri" "cl-annot"))
  (try-asd (concatenate 'string "/home/claude/lf/tars/" n ".tar") n))
(say "LF-END" "asd")
