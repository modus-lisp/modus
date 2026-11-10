;;;; term-swift-check.lisp -- the samples, as Swift encoded them, decoded by the image and compared with the
;;;; samples themselves.  Reads /tmp/term-swift.hex (written by test/term-swift/main.swift).
;;;;   ./modus --script test/term-swift-check.lisp

(load "test/term-harness.lisp")
(load "test/term-samples.lisp")

(let ((lines (read-lines "/tmp/term-swift.hex"))
      (fails 0)
      (i 0))
  (dolist (line lines)
    (let ((back (hex-term line)))
      (if (term= back (nth i *samples*))
          (format t "~&PASS ~D~%" i)
          (progn (incf fails)
                 (format t "~&FAIL ~D: decoded ~S~%" i back))))
    (incf i))
  (format t "~&~D of ~D decoded in the image to their samples~%" (- i fails) i)
  (when (/= i (length *samples*))
    (format t "~&FAIL: ~D lines for ~D samples~%" i (length *samples*))))
