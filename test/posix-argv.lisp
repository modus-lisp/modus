;;;; posix-argv.lisp -- SB-EXT:*POSIX-ARGV* is the command line, as SBCL gives it: element 0 the
;;;; program, then the arguments after the script, SBCL's own options dropped.
;;;;   modus --script test/posix-argv.lisp a "b c" --foo      (also: sbcl --script ...)
(defvar *fail* 0)
(defmacro check (name form) `(unless ,form (incf *fail*) (format t "~&FAIL: ~a~%" ,name)))
(check "bound" (boundp 'sb-ext:*posix-argv*))
(let ((v (and (boundp 'sb-ext:*posix-argv*) sb-ext:*posix-argv*)))
  (format t "~&argv: ~s~%" v)
  (check "a list of strings" (and (consp v) (every #'stringp v)))
  (check "arguments after the script, spaces kept, options passed through"
         (equal (cdr v) '("a" "b c" "--foo")))
  (check "element 0 is the program" (plusp (length (car v)))))
(check "SETF is seen through the same symbol"
       (let ((old sb-ext:*posix-argv*))
         (setf sb-ext:*posix-argv* (list "x"))
         (prog1 (equal (symbol-value (find-symbol "*POSIX-ARGV*" "SB-EXT")) '("x"))
           (setf sb-ext:*posix-argv* old))))
(format t "~&POSIX-ARGV ~a~%" (if (zerop *fail*) "PASS" (format nil "FAIL (~a)" *fail*)))
