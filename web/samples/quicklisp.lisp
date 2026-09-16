;;;; quicklisp.lisp - load a real Common Lisp library with ql:quickload.
;;;;
;;;; ql:quickload is ready at the prompt: the loader (modus-quicklisp/setup.lisp)
;;;; is auto-loaded when the page boots.  It is OFFLINE here -- a "system" is a
;;;; plain .tar bundled under systems/, untarred and loaded by the image's baked
;;;; install-tarball pipeline, so nothing is fetched over the network.
;;;;
;;;; This grabs the :sha1 library and hashes a few strings with it.  Each form
;;;; is read then evaluated in turn, so the (ql:quickload :sha1) that creates the
;;;; SHA1 package runs before any (sha1:...) below is read.

(ql:quickload :sha1)

(terpri)
(format t "sha1(\"abc\")   = ~a~%" (sha1:sha1-hex "abc"))
(format t "sha1(\"hello\") = ~a~%" (sha1:sha1-hex "hello"))
(format t "sha1(\"\")      = ~a~%" (sha1:sha1-hex ""))
(terpri)
(format t "loaded :sha1 from a bundled tar -- now try your own at the prompt:~%")
(format t "  (sha1:sha1-hex \"your string here\")~%")
