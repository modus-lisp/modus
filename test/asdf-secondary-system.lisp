;;;; test/asdf-secondary-system.lisp -- ASDF:FIND-SYSTEM of a secondary system the .asd does
;;;; not define is NIL (with ERROR-P NIL) or MISSING-COMPONENT, as in SBCL's ASDF.  modus used
;;;; to answer the .asd's FIRST system for any "foo/anything".
;;;;   ./modus --script test/asdf-secondary-system.lisp      (SBCL: sbcl --script, same output)
(require :asdf)
(let* ((dir (format nil "~a/asdf-secondary-~d/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp") (random 1000000 (make-random-state t))))
       (asd (concatenate 'string dir "probe-sys.asd")))
  (ensure-directories-exist dir)
  (with-open-file (o asd :direction :output :if-exists :supersede)
    (write-string "(asdf:defsystem \"probe-sys\" :components ())
(asdf:defsystem \"probe-sys/extra\" :depends-on (\"probe-sys\") :components ())
" o))
  (push (pathname dir) asdf:*central-registry*)
  (let ((fails 0))
    (flet ((check (name ok) (format t "~&~:[FAIL~;ok  ~] ~a~%" ok name) (unless ok (incf fails))))
      (check "primary system found" (asdf:find-system "probe-sys" nil))
      (check "defined secondary system found" (asdf:find-system "probe-sys/extra" nil))
      (check "undefined secondary system is NIL" (null (asdf:find-system "probe-sys/nonsense" nil)))
      (check "undefined secondary system signals MISSING-COMPONENT"
             (handler-case (progn (asdf:find-system "probe-sys/nonsense") nil)
               (asdf:missing-component () t))))
    (delete-file asd)
    (format t "~&asdf-secondary-system: ~:[FAIL~;PASS~] (~d failed)~%" (zerop fails) fails)))
