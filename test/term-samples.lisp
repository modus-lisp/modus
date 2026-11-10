;;;; term-samples.lisp -- the terms both sides encode and decode, in the same order as the Swift list in
;;;; test/term-swift/main.swift.  Keep the two in step: the check is position by position.

(defun codes (&rest codes)
  (map 'string #'code-char codes))

(defparameter *samples*
  (list nil 0 42 -7
        "hello" (codes 99 97 102 233) (codes #x65e5 #x672c) (codes #x1f642) ""
        2.5d0 -0.25d0 0.0d0
        :tap
        (list :id "a1" :title "Apples" :count 3 :weight 2.5d0 :kind :fruit)
        (list (codes 99 97 102 233) (codes #x65e5 #x672c) (codes #x1f642) 1.25d0)))
