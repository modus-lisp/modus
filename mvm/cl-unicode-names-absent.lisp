;;;; mvm/cl-unicode-names-absent.lisp -- an image WITHOUT the Unicode character-name table.
;;;;
;;;; Baked instead of mvm/cl-unicode-names.lisp + its generated data when MODUS_UNICODE_NAMES
;;;; is off (the default for bare-metal images, where the ~210 KB is not worth it).  CHAR-NAME
;;;; and NAME-CHAR (mvm/cl-eval.lisp) then know the control characters, SBCL's short aliases,
;;;; U+XXXX / UXXXX and a handful of Unicode names; the reader signals a READER-ERROR naming
;;;; any other name, and saying the table is missing.

(in-package :modus.runtime)

(defun %ucn-present-p () nil)
(defun %ucn-char-name (code) code nil)
(defun %ucn-name-char (name) name nil)
