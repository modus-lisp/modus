;;;; test/deposits-gaps.lisp -- what cl-deposits' suites found wrong on modus, as lines that must
;;;; print the same under SBCL and modus (test/run-deposits-gaps.sh compares them).

;;; 1. SB-MOP:CLASS-SLOTS of a STRUCTURE class.  CLASS-OF a structure answered a class whose
;;;    slot list was NIL, so a deep copy (COPY-STRUCTURE, then a fresh copy of every slot it
;;;    lists) shared every hash table with the original: a ledger "tried" on a copy changed the
;;;    real one, and every operation applied twice.
(setq *print-pretty* nil)   ; line breaking is not what is compared
(defstruct dg-base (a 1) (h (make-hash-table)))
(defstruct (dg-kid (:include dg-base)) (b 2) (c nil))
(format t "class-slots base: ~s~%"
        (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of (make-dg-base)))))
(format t "class-slots kid: ~s~%"
        (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (find-class 'dg-kid))))
(format t "class-direct-slots kid: ~s~%"
        (mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (find-class 'dg-kid))))
(let* ((x (make-dg-kid)) (c (copy-structure x)))
  (dolist (s (sb-mop:class-slots (class-of c)))
    (let ((n (sb-mop:slot-definition-name s)))
      (when (hash-table-p (slot-value c n)) (setf (slot-value c n) (make-hash-table)))))
  (setf (gethash 1 (dg-base-h c)) :copy)
  (format t "deep copy shares the table: ~s~%" (eq (dg-base-h x) (dg-base-h c)))
  (format t "original untouched: ~s~%" (gethash 1 (dg-base-h x))))

;;; 2. Hash-table iteration order is insertion order, as on SBCL (a JSON object built in an
;;;    EQUAL table serialises its keys in the order they were stored).  Every walker agrees.
(defun dg-walks (h)
  (let ((m '()) (l '()) (w '()) (f '()))
    (maphash (lambda (k v) v (push k m)) h)
    (let ((fn (lambda (k v) v (push k f)))) (maphash fn h))
    (loop for k being the hash-keys of h using (hash-value v) do (progn v (push k l)))
    (with-hash-table-iterator (next h)
      (loop (multiple-value-bind (more k) (next) (unless more (return)) (push k w))))
    (mapcar #'reverse (list m f l w))))
(let ((h (make-hash-table :test #'equal)))
  (dolist (k '("proof_type" "accused" "ledger_id" "evidence")) (setf (gethash k h) 1))
  (format t "small equal table: ~s~%" (dg-walks h)))
(let ((h (make-hash-table :test #'eql)))
  (dotimes (i 100) (setf (gethash (* 7 i) h) i))
  (format t "large eql table: ~s~%" (dg-walks h))
  (format t "loop hash-values: ~s~%" (loop for v being the hash-values of h collect v)))
(let ((h (make-hash-table :test #'equal)))
  (dotimes (i 60) (setf (gethash (format nil "k~d" i) h) i))
  (setf (gethash "k3" h) :again)                ; a new value keeps the key's place
  (format t "large equal table: ~s~%" (first (dg-walks h))))

;;; 3. #'NAME is NAME's definition when the form is EVALUATED.  For an interpreted runtime DEFUN
;;;    the JIT baked a late-binding thunk instead, so the wrapper idiom below called ITSELF until
;;;    the stack was gone (cl-deposits' index-test wraps its harness's REPORT this way).
(load (merge-pathnames "deposits-gaps-defs.lisp" *load-truename*))
(defvar *dg-calls* 0)
(let ((old #'dg-report))
  (setf (fdefinition 'dg-report)
        (lambda () (when (> (incf *dg-calls*) 5) (error "recursed")) (list :wrapped (funcall old)))))
(format t "wrapped once: ~s ~s~%" (dg-report) *dg-calls*)
(let ((before #'dg-report))
  (format t "#'NAME is the current definition: ~s~%" (eq before (fdefinition 'dg-report))))
