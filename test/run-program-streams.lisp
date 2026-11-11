;;;; run-program-streams.lisp -- SB-EXT:RUN-PROGRAM with Lisp streams as :INPUT,
;;;; :OUTPUT and :ERROR (cram's tar oracle runs GNU tar through exactly this).
;;;; Run: ./modus --script test/run-program-streams.lisp   (prints PASS or FAIL)
(defvar *fails* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error (e) (list :error (princ-to-string e))))))
     (unless (equal got ,expect) (incf *fails*))
     (format t "~:[FAIL~;ok  ~] ~a: ~s~%" (equal got ,expect) ,name
             (if (and (stringp got) (> (length got) 60)) (length got) got))))
(defun sh (cmd &rest keys)
  (with-output-to-string (out)
    (apply #'sb-ext:run-program "/bin/sh" (list "-c" cmd) :output out :search nil keys)))
(check ":output to a string" (sh "echo hello; echo world") (format nil "hello~%world~%"))
(check ":input from a string"
       (sh "tr a-z A-Z" :input (make-string-input-stream (format nil "abc~%def~%")))
       (format nil "ABC~%DEF~%"))
(check ":error to its own stream"
       (let ((err (make-string-output-stream)))
         (list (sh "echo out; echo err 1>&2" :error err) (get-output-stream-string err)))
       (list (format nil "out~%") (format nil "err~%")))
(check ":error :output joins them" (sh "echo a 1>&2; echo b" :error :output) (format nil "a~%b~%"))
(check ":error nil discards" (sh "echo a 1>&2; echo b" :error nil) (format nil "b~%"))
(check "1 MB on stdout and stderr at once"
       (let ((err (make-string-output-stream)))
         (list (length (sh "head -c 1048576 /dev/zero | tr '\\0' x; head -c 1048576 /dev/zero | tr '\\0' y 1>&2" :error err))
               (length (get-output-stream-string err))))
       '(1048576 1048576))
(check "exit code still reported"
       (sb-ext:process-exit-code (sb-ext:run-program "/bin/sh" '("-c" "exit 3") :output (make-string-output-stream)))
       3)
#+modus
(check ":wait nil with a Lisp stream is refused"
       (handler-case (progn (sb-ext:run-program "/bin/true" nil :wait nil :output (make-string-output-stream)) :ran)
         (error () :refused))
       :refused)
#+modus
(check "no temporary files left behind"
       (directory "/tmp/modus-run-program-*")
       nil)
(format t "~a~%" (if (zerop *fails*) "PASS" (format nil "FAIL (~d)" *fails*)))
