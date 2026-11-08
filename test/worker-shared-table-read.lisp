;;;; worker-shared-table-read.lisp — a worker may READ a table main made, even when
;;;; the read is the one that builds the table's index.
;;;;
;;;;   ./modus --script test/worker-shared-table-read.lisp
;;;;
;;;; GETHASH builds a table's index lazily, on the first lookup once the table holds
;;;; 32 entries (a table that reached exactly 32 by PUTHASH has none yet).  On a
;;;; worker that build stored the worker's own index into main's table, and the
;;;; shared-store guard refused it.  CLOS's *GF-FIND-MEMO* is such a table: once a
;;;; worker's lookup tripped it, every generic call on that thread that missed its
;;;; dispatch cache failed, so a compositor thread could run no CLOS app.

(defvar *fail* 0)
(defun chk (name got want)
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defun on-worker (fn)
  (sb-thread:join-thread
   (sb-thread:make-thread
    (lambda () (handler-case (funcall fn) (error (e) (list :error (princ-to-string e))))))))

(defun table-of-32 (test keyfn)
  (let ((h (make-hash-table :test test)))
    (dotimes (i 32 h) (setf (gethash (funcall keyfn i) h) i))))

(let ((h (table-of-32 'equal (lambda (i) (format nil "k~D" i)))))
  (chk "EQUAL table of 32, first read on a worker"
       (on-worker (lambda () (multiple-value-list (gethash "k5" h)))) '(5 t))
  (chk "the same table afterwards, on main" (gethash "k31" h) 31))

(let ((h (table-of-32 'eq (lambda (i) (intern (format nil "WSTR-~D" i))))))
  (chk "EQ symbol table of 32, first read on a worker"
       (on-worker (lambda () (list (gethash 'wstr-7 h) (gethash 'wstr-99 h :none))))
       '(7 :none)))

(let ((h (table-of-32 'eql #'identity)))
  (chk "EQL fixnum table of 32, a miss first on a worker"
       (on-worker (lambda () (list (gethash 1000 h :none) (gethash 3 h)))) '(:none 3)))

;; Two workers racing to build the same index.
(let* ((h (table-of-32 'equal (lambda (i) (format nil "r~D" i))))
       (ths (loop repeat 4 collect
                  (sb-thread:make-thread
                   (lambda () (handler-case (loop for i below 32 sum (gethash (format nil "r~D" i) h))
                                (error (e) (princ-to-string e))))))))
  (chk "four workers build one index" (mapcar #'sb-thread:join-thread ths) '(496 496 496 496)))

;; Generic functions on a worker while main's GF memo crosses the threshold.
(defgeneric wstr-gf-0 (x))
(defmethod wstr-gf-0 ((x integer)) (list :int x))
(defmethod wstr-gf-0 ((x string)) (list :str x))
(defvar *gfs*
  (loop for i from 1 to 40
        collect (let ((name (intern (format nil "WSTR-GF-~D" i))))
                  (eval `(defgeneric ,name (x)))
                  (eval `(defmethod ,name ((x t)) (list ,i x)))
                  name)))
(let ((bad nil))
  (dolist (g *gfs*)
    (funcall g 0)                       ; main: one more memo entry
    ;; the worker looks G up in the memo main just grew (its first hit there)
    (let ((r (on-worker (lambda () (list (funcall g 1) (wstr-gf-0 1) (wstr-gf-0 "a"))))))
      (unless (equal r (list (list (parse-integer (symbol-name g) :start 8) 1) '(:int 1) '(:str "a")))
        (push (list g r) bad))))
  (chk "generic calls on workers as the GF memo grows past 32" bad nil))

(format t "~%WORKER-SHARED-TABLE-READ: ~:[FAIL~;PASS~]~%" (zerop *fail*))
