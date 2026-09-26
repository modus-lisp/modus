;;; INCF/DECF/PUSH/POP/PUSHNEW evaluate each subform of the place ONCE, left
;;; to right (CLHS 5.1.3); GET-SETF-EXPANSION returns real temporaries.
(defparameter *ok*
  (and (equalp (let ((x (vector nil nil nil nil)) (y (vector 'a 'b 'c 'd)) (i 1))
                (push (aref y (incf i)) (aref x (incf i)))
                (list x y i))
              '(#(nil nil nil (c)) #(a b c d) 3))
       (equalp (let ((v (vector 5 5)) (i -1)) (incf (aref v (incf i)) 10) (list v i)) '(#(15 5) 0))
       (equalp (let ((v (vector 5 5)) (i -1)) (decf (aref v (incf i)) 2) (list v i)) '(#(3 5) 0))
       (equalp (let ((v (vector (list 1 2) nil)) (i -1)) (list (pop (aref v (incf i))) v i)) '(1 #((2) nil) 0))
       (equalp (let ((v (vector nil)) (i -1)) (pushnew 1 (aref v (incf i))) (pushnew 1 (aref v (progn (setq i -1) (incf i)))) v) '#((1)))
       (let ((vars (get-setf-expansion '(aref x (incf i))))) (= (length vars) 2))))
(format t "~&place-subforms-once: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
