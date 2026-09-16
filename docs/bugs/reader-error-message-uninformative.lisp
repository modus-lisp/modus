;;;; Minimal repro for docs/bugs/reader-error-message-uninformative.md
;;;; Load in any Modus image; compare the printed error to SBCL's.
;;;;   Modus: #(READER-ERROR ((STREAM)))
;;;;   SBCL:  end of file on #<...STRING-INPUT-STREAM... from "(a (b c)">
(handler-case (read-from-string "(a (b c)")   ; one ')' missing on purpose
  (error (e) (format t "reader error: ~a~%" e)))
