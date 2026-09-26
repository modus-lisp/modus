;;; REQUIRE loads the given pathnames when the module is absent (it was a
;;; silent no-op), treats a built-in package's name as present, and signals
;;; for an unknown module with no pathnames.
(defparameter *f* (format nil "/tmp/modus-require-test-~d.lsp" (get-universal-time)))
(with-open-file (s *f* :direction :output :if-exists :supersede)
  (write-string "(defun req-test-fun () :loaded)" s))
(defparameter *ok*
  (and (progn (require "REQ-TEST-MOD" (pathname *f*)) (eq (funcall 'req-test-fun) :loaded))
       (null (require :asdf))
       (eq (handler-case (require "NO-SUCH-MODULE-XYZ") (error () :err)) :err)))
(ignore-errors (delete-file *f*))
(format t "~&require-pathnames: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
