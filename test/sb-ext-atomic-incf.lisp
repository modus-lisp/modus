;;;; test/sb-ext-atomic-incf.lisp -- SB-EXT:WORD and SB-EXT:ATOMIC-INCF /
;;;; ATOMIC-DECF: SBCL's spelling, SBCL's return value (the OLD value), on a
;;;; WORD struct slot, a vector element and a CAR -- and four threads
;;;; hammering one slot lose no update.
;;;;
;;;;   ./modus --script test/sb-ext-atomic-incf.lisp

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (incf *pass*)
      (progn (incf *fail*) (format t "~&FAIL ~A: got ~S want ~S~%" name got want))))

(defstruct counter (n 0 :type sb-ext:word))
(let ((x (make-counter)) (v (make-array 3 :element-type 'sb-ext:word :initial-element 0)) (c (list 5)))
  (check "incf returns old" (list (sb-ext:atomic-incf (counter-n x)) (sb-ext:atomic-incf (counter-n x) 10) (counter-n x)) '(0 1 11))
  (check "decf returns old" (list (sb-ext:atomic-decf (counter-n x) 2) (counter-n x)) '(11 9))
  (check "aref" (list (sb-ext:atomic-incf (aref v 1)) (aref v 1)) '(0 1))
  (check "car" (list (sb-ext:atomic-decf (car c)) c) '(5 (4)))
  (check "word type" (list (typep 5 'sb-ext:word) (typep -1 'sb-ext:word) (typep (expt 2 64) 'sb-ext:word)
                           (typep (1- (expt 2 64)) 'sb-ext:word))
         '(t nil nil t)))

(defvar *shared* (make-counter))
(defun bump (n) (dotimes (i n) (sb-ext:atomic-incf (counter-n *shared*))) n)
(let ((threads (loop repeat 4 collect (sb-thread:make-thread #'bump :arguments (list 20000)))))
  (mapc #'sb-thread:join-thread threads)
  (check "4 threads x 20000 increments" (counter-n *shared*) 80000))

(format t "~&sb-ext-atomic-incf: ~D passed, ~D failed~%" *pass* *fail*)
(format t "~&~:[FAIL~;PASS~]~%" (zerop *fail*))
