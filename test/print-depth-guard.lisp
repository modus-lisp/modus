;;; print-depth-guard.lisp — the printer signals an ERROR instead of overflowing
;;; the stack on structure nested past +%write-depth-limit+ (1000 levels), the
;;; shape a car-cyclic object takes when printed with *PRINT-CIRCLE* NIL.
;;; Before the guard: SIGSEGV (upstream ansi-test shard 26 died at
;;; PRINT.CONS.RANDOM.2's failure report).  Run: ./modus --script test/print-depth-guard.lisp
(defvar *fails* 0)
(defun check (name ok) (format t "~a ~a~%" (if ok "PASS" "FAIL") name) (unless ok (setq *fails* (+ *fails* 1))))
(defun nested (n) (let ((x nil)) (dotimes (i n) (setq x (list x))) x))
(check "800-deep list prints" (= (length (prin1-to-string (nested 800))) 1603))
(check "1500-deep list signals ERROR" (eq :err (handler-case (progn (prin1-to-string (nested 1500)) :printed) (error (c) :err))))
(check "car-cycle with *print-circle* NIL signals ERROR"
       (eq :err (handler-case (let ((x (list 1 2))) (setf (car x) x) (prin1-to-string x) :printed) (error (c) :err))))
(check "car-cycle with *print-circle* T prints" (let ((x (list 1 2))) (setf (car x) x) (string= (let ((*print-circle* t)) (prin1-to-string x)) "#1=(#1# 2)")))
(check "depth restored after the error" (= (length (prin1-to-string (nested 800))) 1603))
(format t "~a~%" (if (zerop *fails*) "ALL PASS" (format nil "~a FAILED" *fails*)))
(sys-exit (if (zerop *fails*) 0 1))
