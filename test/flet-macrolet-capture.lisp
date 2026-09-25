;;;; test/flet-macrolet-capture.lisp -- a call to a CAPTURING local function
;;;; that only appears in a MACROLET expansion, plus WITH-HASH-TABLE-ITERATOR,
;;;; EQUAL on bit vectors, STRINGP of a displaced string, SXHASH of keywords.
;;;;   ./modus --script test/flet-macrolet-capture.lisp     ; exit 0 = PASS
(defvar *fmc-fail* 0)
(defun chk (name got want)
  (unless (equal got want) (setq *fmc-fail* (+ *fmc-fail* 1)))
  (format t "~&~a ~a got=~s~%" (if (equal got want) "ok  " "FAIL") name got))
(chk "flet-capture-via-macrolet"
     (let ((n 0)) (flet ((bump () (setq n (+ n 1)))) (macrolet ((b () '(bump))) (b) (b) n)))
     2)
(chk "whti-all"
     (let ((h (make-hash-table)) (acc nil))
       (dotimes (i 5) (setf (gethash i h) (* i i)))
       (with-hash-table-iterator (it h)
         (loop (multiple-value-bind (more k v) (it)
                 (unless more (return))
                 (push (+ k v) acc))))
       (sort acc #'<))
     '(0 2 6 12 20))
(chk "whti-empty" (multiple-value-list (with-hash-table-iterator (it (make-hash-table)) (it))) '(nil))
(chk "equal-bitvec" (equal #*1011 (copy-seq #*1011)) t)
(chk "equal-bitvec-ne" (equal #*1011 #*1010) nil)
(chk "stringp-displaced"
     (stringp (make-array 3 :element-type 'character
                            :displaced-to (make-array 7 :element-type 'character :initial-contents "xxabcyy")
                            :displaced-index-offset 2))
     t)
(chk "sxhash-keyword" (= (sxhash :foo) (sxhash :foo)) t)
(format t "~&~a (~d failures)~%" (if (zerop *fmc-fail*) "PASS" "FAIL") *fmc-fail*)
(sys-exit (if (zerop *fmc-fail*) 0 1))
