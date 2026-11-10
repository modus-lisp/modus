;;;; closure-place-assign.lisp — an assignment through a PLACE inside a closure
;;;; reaches the variable the place's expansion assigns.
;;;;
;;;;   ./modus --script test/closure-place-assign.lisp
;;;;
;;;; (setf (getf out k) v) is (setq out (set-getf out k v)): it assigns OUT, but
;;;; only its expansion says so.  The closure-capture scan looked only at the
;;;; source form, saw a SETF of a non-variable place, and did not box OUT, so the
;;;; FLET helper assigned its own copy and the outer OUT stayed NIL (glass's
;;;; %PARSE-PARAMS built no audio parameters).

(defvar *fail* 0)
(defun chk (name got want)
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defun via-flet () (let ((out '())) (flet ((tok () (setf (getf out :a) 1))) (tok)) out))
(defun via-labels () (let ((out '())) (labels ((tok () (setf (getf out :a) 1))) (tok)) out))
(defun via-lambda () (let ((out '())) (funcall (lambda () (setf (getf out :a) 1))) out))
(defun via-incf () (let ((out '())) (flet ((tok () (incf (getf out :n 10)))) (tok) (tok)) out))
(defun via-push () (let ((out '())) (flet ((tok (x) (push x (getf out :l)))) (tok 1) (tok 2)) out))
(defun via-multi () (let ((a '()) (b '()))
                      (flet ((tok () (setf (getf a :x) 1 (getf b :y) 2))) (tok)) (list a b)))
(defun via-values () (let ((p 0) (q 0))
                       (flet ((tok () (setf (values p q) (values 3 4)))) (tok)) (list p q)))
(defun parse-params (line)              ; glass's shape
  (let ((out '()) (start 0))
    (flet ((tok (s e)
             (let* ((w (subseq line s e)) (eq (position #\= w)))
               (when eq
                 (setf (getf out (intern (string-upcase (subseq w 0 eq)) :keyword))
                       (parse-integer w :start (1+ eq)))))))
      (loop for i = (position #\Space line :start start)
            do (tok start (or i (length line)))
               (if i (setf start (1+ i)) (return))))
    out))

(chk "SETF GETF in an FLET" (via-flet) '(:a 1))
(chk "SETF GETF in a LABELS" (via-labels) '(:a 1))
(chk "SETF GETF in a LAMBDA" (via-lambda) '(:a 1))
(chk "INCF GETF in an FLET" (via-incf) '(:n 12))
(chk "PUSH onto a GETF in an FLET" (via-push) '(:l (2 1)))
(chk "two GETF places in one SETF" (via-multi) '((:x 1) (:y 2)))
(chk "SETF VALUES in an FLET" (via-values) '(3 4))
(chk "a toplevel LET" (let ((out '())) (flet ((tok () (setf (getf out :a) 1))) (tok)) out) '(:a 1))
(chk "glass's parameter parser" (parse-params "rate=48000 channels=2") '(:channels 2 :rate 48000))

(format t "~%CLOSURE-PLACE-ASSIGN: ~:[FAIL~;PASS~]~%" (zerop *fail*))
