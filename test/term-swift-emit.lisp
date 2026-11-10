;;;; term-swift-emit.lisp -- the samples, encoded by the image, one hex line each, for the Swift check.
;;;;   ./modus --script test/term-swift-emit.lisp        (writes /tmp/term-lisp.hex)

(load "test/term-harness.lisp")
(load "test/term-samples.lisp")

(with-open-file (out "/tmp/term-lisp.hex" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (dolist (s *samples*)
    (write-line (term-hex s) out)))
(format t "~&emitted ~D samples~%" (length *samples*))
