;;;; jit-census.lisp — WHY does an mvm-eval form fall back to the interpreter?
;;;;
;;;; The seam already counted HOW OFTEN (*jit-native-count* / *jit-fallback-count*).
;;;; mvm/mvm-eval.lisp now also tallies a REASON for every fallback, and, when
;;;; *jit-census-on* is set, the NAMES behind each blocked relocation and the
;;;; MESSAGES behind each translator gap.  This script turns those tallies into
;;;; a ranked blocker list.
;;;;
;;;; Every shape that still falls back is a BLOCKER, not an acceptable steady
;;;; state: the interpreter treats +op-yield+ as a NO-OP (mvm/interp.lisp), so
;;;; cooperative scheduling cannot work under it — an actor whose body is
;;;; interpreted can never yield.  Cooperative threading therefore requires the
;;;; JIT, and anything on this list is between us and threading.
;;;;
;;;; Run:  ./modus --load tests/jit-census.lisp --quit

(setq *jit-census-on* t)

(defparameter *jc-workload*
  '(
    ;; straight-line arithmetic / logic — expected fully native
    (+ 1 2) (* 6 7) (- 10 3) (logand 12 10) (ash 1 20) (expt 2 40)
    (let ((x 5)) (* x x))
    (let ((s 0)) (dotimes (i 100 s) (setq s (+ s i))))
    ;; multiple-value producers
    (floor 17 5) (truncate 17 5) (round 17 5) (mod 17 5) (values 1 2)
    (multiple-value-list (floor 7 2))
    ;; sequences / strings / aggregates
    (length "hello") (subseq "hello" 1 3) (concatenate 'string "a" "b")
    (mapcar #'1+ (list 1 2 3)) (sort (list 3 1 2) #'<) (reduce #'+ (list 1 2 3))
    (make-array 4 :initial-element 0) (vector 1 2 3)
    (let ((h (make-hash-table))) (setf (gethash 1 h) 2) (gethash 1 h))
    ;; control flow / conditions
    (block b (return-from b 1)) (catch 'c (throw 'c 1))
    (handler-case (error "x") (error (c) 1))
    (unwind-protect 1 2)
    ;; closures
    (let ((n 1)) (funcall (lambda (x) (+ n x)) 2))
    (labels ((f (n) (if (< n 2) 1 (* n (f (- n 1)))))) (f 8))
    (flet ((f (x) (* x 2))) (f 3))
    ;; printing / reading
    (format nil "~D" 42) (prin1-to-string (list 1 2)) (read-from-string "(1)")
    ;; CLOS
    (progn (defclass jc-c () ((s :initform 1 :accessor jc-s)))
           (jc-s (make-instance 'jc-c)))
    (progn (defclass jc-d () ()) (defmethod jc-m ((x jc-d)) 1)
           (jc-m (make-instance 'jc-d)))
    ))

;; Cross-form runtime definitions: the library / actor-entry shape.
(defun jc-f1 (x) (* x 3))
(defun jc-f2 (x) (+ (jc-f1 x) 1))

(defparameter *jc-workload2*
  '((jc-f1 1) (jc-f2 2) (mapcar #'jc-f1 (list 1 2)) (funcall #'jc-f1 3)
    (let ((s 0)) (dotimes (i 20 s) (setq s (+ s (jc-f1 i)))))))

(defun jc-n (x) (if (and (boundp x) (symbol-value x)) (symbol-value x) 0))

(defun jc-list (x)
  "The alist held in special X, or NIL.  NB: jc-n returns the FIXNUM 0 for an
   unbound/NIL special and 0 is TRUE in CL, so it must never be used as the
   test of a list-or-empty branch."
  (if (boundp x) (symbol-value x) nil))

(defun jc-run (forms)
  (dolist (f forms)
    (handler-case (eval f) (t (c) nil))))

(defun jc-report (tag)
  (format t "~%=== JIT CENSUS: ~A ===~%" tag)
  (let* ((nat (jc-n '*jit-native-count*))
         (fb  (jc-n '*jit-fallback-count*))
         (tot (+ nat fb)))
    (format t "NATIVE=~D  FALLBACK=~D  TOTAL=~D  NATIVE%=~D~%"
            nat fb tot (if (> tot 0) (floor (* 100 nat) tot) 0)))
  (format t "-- fallback reasons (ranked) --~%")
  (dolist (pr (sort
               (list (cons "R-MV                    (form left MV state native cannot hand back)"
                           (jc-n '*jit-mv-fallback-count*))
                     (cons "R-TRANSLATE-ERR         (translator gap: translate-mvm-to-* signalled)"
                           (jc-n '*jit-translate-err-count*))
                     (cons "R-RELOC-CALL-NONNATIVE  (callee is a runtime heap closure, no PROT_EXEC)"
                           (jc-n '*jit-r-reloc-call-nonnative*))
                     (cons "R-RELOC-CALL-UNRESOLVED (callee name not resolvable at all)"
                           (jc-n '*jit-r-reloc-call-unresolved*))
                     (cons "R-RELOC-FNADDR-FAIL     (#'NAME value load unresolvable)"
                           (jc-n '*jit-r-reloc-fnaddr-fail*))
                     (cons "R-MMAP-FAIL             (exec page could not be mapped)"
                           (jc-n '*jit-r-mmap-fail*))
                     (cons "R-NATIVE-ESCAPE         (native RAN then escaped: DOUBLE-EXECUTES)"
                           (jc-n '*jit-r-native-escape*))
                     (cons "R-PAGE-NIL              (page build returned NIL; union of the RELOC/MMAP rows)"
                           (jc-n '*jit-r-page-nil*)))
               #'> :key #'cdr))
    (format t "  ~6D  ~A~%" (cdr pr) (car pr)))
  (format t "-- blocked out-of-module CALL targets --~%")
  (if (jc-list '*jit-blocked-callees*)
      (dolist (pr (sort (copy-list (jc-list '*jit-blocked-callees*)) #'> :key #'cdr))
        (format t "  ~6D  ~A~%" (cdr pr) (car pr)))
      (format t "  (none)~%"))
  (format t "-- blocked #'NAME fn-addr loads --~%")
  (if (jc-list '*jit-blocked-fnaddrs*)
      (dolist (pr (sort (copy-list (jc-list '*jit-blocked-fnaddrs*)) #'> :key #'cdr))
        (format t "  ~6D  ~A~%" (cdr pr) (car pr)))
      (format t "  (none)~%"))
  (format t "-- translator gaps (message . count) --~%")
  (if (jc-list '*jit-translate-err-msgs*)
      (dolist (pr (sort (copy-list (jc-list '*jit-translate-err-msgs*)) #'> :key #'cdr))
        (format t "  ~6D  ~A~%" (cdr pr) (car pr)))
      (format t "  (none)~%")))

(format t "JC-START jit-enabled=~A~%"
        (handler-case (if (%jit-enabled-p) 1 0) (t (c) :err)))

(jc-run *jc-workload*)
(jc-run *jc-workload2*)
(jc-report "in-process eval workload")
(format t "~%JC-DONE~%")
