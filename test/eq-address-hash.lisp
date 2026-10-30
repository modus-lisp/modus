;;;; test/eq-address-hash.lisp -- EQ/EQL hash tables index object keys by
;;;; address, and stay right across collections that move them.
;;;;
;;;;   ./modus --script test/eq-address-hash.lisp
;;;;
;;;; An object key (cons, string, vector, struct, instance, function) of an
;;;; EQ/EQL table used to be unhashable, and one of them sent the whole table to
;;;; a linear scan for good (*PRINT-CIRCLE*'s table: quadratic printing).  Now
;;;; such keys hash by address in region 0 and the index is rebuilt after a
;;;; collection (mvm/prelude.lisp, ADDRESS INDEX).  Every check here holds
;;;; whether or not the index is used; the timing check is the one that does not.

(defvar *fails* 0)
(defun chk (name ok)
  (format t "~&  ~a ~a~%" (if ok "ok  " "FAIL") name)
  (unless ok (setq *fails* (+ *fails* 1))))
(defun epoch () (modus-int::%gc-epoch))
(defun collect () (let ((e (epoch))) (sb-ext:gc :full t) (/= e (epoch))))
(defstruct pt x y)

(defun fill-table (test n)
  (let ((h (make-hash-table :test test)) (keys (make-array n)))
    (dotimes (i n)
      (let ((k (case (mod i 5)
                 (0 (cons i i)) (1 (format nil "s~d" i)) (2 (vector i))
                 (3 (make-pt :x i)) (t (let ((j i)) (lambda () j))))))
        (setf (aref keys i) k (gethash k h) i)))
    (values h keys)))
(defun all-found (h keys)
  (let ((ok t))
    (dotimes (i (length keys) ok)
      (multiple-value-bind (v p) (gethash (aref keys i) h)
        (unless (and p (eql v i)) (setq ok nil))))))
(defun none-found (h n)
  "Fresh objects equal in content to the keys are NOT EQ to them."
  (let ((ok t))
    (dotimes (i n ok)
      (when (nth-value 1 (gethash (cons i i) h)) (setq ok nil))
      (when (nth-value 1 (gethash (format nil "s~d" i) h)) (setq ok nil)))))

(dolist (test '(eq eql))
  (multiple-value-bind (h keys) (fill-table test 5000)
    (format t "~&~a table, 5000 object keys~%" test)
    (chk "every key found" (all-found h keys))
    (chk "content-equal fresh objects are not keys" (none-found h 100))
    (chk "a collection really moved region 0" (collect))
    (chk "every key found after it (index rebuilt)" (all-found h keys))
    (chk "... and after another" (and (collect) (all-found h keys)))
    ;; updates, removals and inserts, with collections between them
    (dotimes (i 1000) (setf (gethash (aref keys i) h) (- i)))
    (collect)
    (chk "updates took" (let ((ok t)) (dotimes (i 1000 ok) (unless (eql (gethash (aref keys i) h) (- i)) (setq ok nil)))))
    (dotimes (i 1000) (setf (gethash (aref keys i) h) i))
    (dotimes (i 500) (remhash (aref keys (* 2 i)) h))
    (collect)
    (chk "removed keys are gone, the rest remain"
         (let ((ok t))
           (dotimes (i 1000 ok)
             (let ((p (nth-value 1 (gethash (aref keys i) h))))
               (unless (eq p (oddp i)) (setq ok nil))))))
    (chk "count agrees" (= (hash-table-count h) 4500))
    (let ((more (make-array 3000)))
      (dotimes (i 3000)
        (setf (aref more i) (cons :new i) (gethash (aref more i) h) (+ 10000 i))
        (when (zerop (mod i 700)) (collect)))
      (chk "inserts across collections all findable"
           (let ((ok t)) (dotimes (i 3000 ok) (unless (eql (gethash (aref more i) h) (+ 10000 i)) (setq ok nil))))))
    (chk "maphash sees every entry" (let ((n 0)) (maphash (lambda (k v) (declare (ignore k v)) (incf n)) h) (= n 7500)))
    (clrhash h)
    (chk "clrhash empties it" (and (zerop (hash-table-count h)) (not (nth-value 1 (gethash (aref keys 1) h)))))
    (setf (gethash (aref keys 1) h) :back)
    (chk "and it fills again" (eq (gethash (aref keys 1) h) :back))))

(format t "~&mixed keys~%")
(let ((h (make-hash-table :test 'eql)) (c (cons 1 2)))
  (setf (gethash 7 h) :fix (gethash #\a h) :char (gethash 'sym h) :sym
        (gethash c h) :cons (gethash (expt 2 70) h) :big (gethash 1.5d0 h) :dbl)
  (dotimes (i 100) (setf (gethash (list i) h) i))
  (collect)
  (chk "fixnum, char, symbol, cons, bignum and float keys together"
       (equal (list (gethash 7 h) (gethash #\a h) (gethash 'sym h) (gethash c h)
                    (gethash (expt 2 70) h) (gethash 1.5d0 h))
              '(:fix :char :sym :cons :big :dbl))))

(format t "~&a worker reads a main-thread table~%")
(multiple-value-bind (h keys) (fill-table 'eq 2000)
  (collect)
  (let ((th (sb-thread:make-thread (lambda () (if (all-found h keys) :ok :missing)))))
    (chk "every key found from the worker (linear path there)" (eq (sb-thread:join-thread th) :ok)))
  (chk "and still from main" (all-found h keys)))

(format t "~&*print-circle*~%")
(let* ((shared (list 1 2 3)) (tree (list shared shared (list shared))))
  (chk "shared structure is labelled"
       (string= (let ((*print-circle* t)) (prin1-to-string tree)) "(#1=(1 2 3) #1# (#1#))")))
(defun big-tree (d) (if (zerop d) (list 'a 'b) (list (big-tree (1- d)) (big-tree (1- d)) d)))
(let ((tr (big-tree 13)) (t0 (get-internal-real-time)))
  (let ((*print-circle* t)) (prin1-to-string tr))
  (let ((s (/ (- (get-internal-real-time) t0) (float internal-time-units-per-second))))
    (format t "~&  (*print-circle* over ~d conses: ~,2f s)~%" (* 4 (expt 2 13)) s)
    (chk "printing 32k conses with *print-circle* is not quadratic (< 2 s)" (< s 2))))

(format t "~&EQ-ADDRESS-HASH: ~a failed~%" *fails*)
(sys-exit (if (zerop *fails*) 0 1))
