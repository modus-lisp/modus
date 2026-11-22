;;;; test/hosted-coroutines.lisp — net/hosted-coroutines.lisp, stage 1
;;;; (docs/coroutines.md).  Run: ./modus --load test/hosted-coroutines.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defvar *main* 0)
(defvar *probe* :global)
(defvar *log* nil)

;; 1. A coroutine that answers every value with its double, forever.
(defun doubler (v)
  (loop (setq v (coroutine-transfer *main* (* 2 v)))))

(coroutines-start)
(setq *main* (coroutine-current))
(check "the main computation is coroutine 0" (eql *main* 0))
(let ((d (coroutine-create #'doubler)))
  (check "a first transfer hands the coroutine its argument and returns its answer"
         (eql (coroutine-transfer d 21) 42))
  (check "and a later transfer resumes it where it transferred out"
         (equal (loop for i from 1 to 50 collect (coroutine-transfer d i))
                (loop for i from 1 to 50 collect (* 2 i)))))

;; 2. Objects held only on a PARKED stack survive collections made elsewhere.
;; Reading the list back intact is not enough on its own: a copying collector
;; leaves an unrooted object readable in the old space until something reuses
;; it (measured: with the parked entry cleared the list still "read intact").
;; What tells a root from a stale pointer is that the collector MOVED the list
;; and updated the parked stack's slot, so its address changes.
(defun holder (n)
  (let ((mine (loop for i below n collect (list i (format nil "s~d" i)))))
    (loop
      (let ((cmd (coroutine-transfer *main* (%val->word mine))))
        (when (eq cmd :check)
          (coroutine-transfer *main*
                              (list (%val->word mine)
                                    (and (= (length mine) n)
                                         (loop for (i s) in mine
                                               always (string= s (format nil "s~d" i)))))))))))

(let* ((h (coroutine-create #'holder))
       (before (coroutine-transfer h 2000)))
  ;; churn and collect from MAIN while H is parked holding its list
  (dotimes (round 4)
    (dotimes (k 30) (let ((junk (make-list 20000 :initial-element (list k round)))) (setq *log* (length junk))))
    (sb-ext:gc :full t))
  (let ((after (coroutine-transfer h :check)))
    (check "a list only a parked coroutine's stack holds is moved by a collection made by another"
           (/= before (first after)))
    (check "and the parked stack's slot names it, intact, where it went" (eq (second after) t))))

;; 3. A collection made INSIDE a coroutine keeps main's parked stack.
(defun collector (v)
  (declare (ignore v))
  (dotimes (k 30) (let ((junk (make-list 20000 :initial-element k))) (setq *log* (length junk))))
  (sb-ext:gc :full t)
  (coroutine-transfer *main* :collected)
  :finished)

(let* ((mine (loop for i below 3000 collect (cons i (* i i))))
       (before (%val->word mine)))
  (let ((c (coroutine-create #'collector)))
    (check "a coroutine can cons and collect" (eq (coroutine-transfer c nil) :collected))
    (check "and main's parked locals were moved by it, not left behind" (/= before (%val->word mine)))
    (check "and came through it intact"
           (loop for (i . sq) in mine always (= sq (* i i))))
    (check "a coroutine that returns hands its value to its resumer"
           (eq (coroutine-transfer c nil) :finished))
    (check "and is then done" (coroutine-done-p c))))

;; 4. Dynamic bindings belong to the computation that made them.
(defun binder (v)
  (declare (ignore v))
  (let ((*probe* :inside))
    (coroutine-transfer *main* *probe*)
    (coroutine-transfer *main* *probe*))
  (coroutine-transfer *main* *probe*))

(let ((b (coroutine-create #'binder)))
  (check "a coroutine sees its own binding" (eq (coroutine-transfer b nil) :inside))
  (check "and main does not see it while the coroutine is parked inside it" (eq *probe* :global))
  (check "and the coroutine still does when resumed" (eq (coroutine-transfer b nil) :inside))
  (check "and once it unbinds it sees the global value" (eq (coroutine-transfer b nil) :global)))

;; 5. Conditions and non-local exits stay inside the coroutine.
(defun thrower (v)
  (declare (ignore v))
  (coroutine-transfer *main*
                      (list (handler-case (error "inside") (error (e) (princ-to-string e)))
                            (catch :tag (throw :tag :thrown))
                            (block b (unwind-protect (return-from b :returned) (push :cleaned *log*)))))
  (coroutine-transfer *main* :again))

(setq *log* nil)
(let ((th (coroutine-create #'thrower)))
  (check "HANDLER-CASE, CATCH/THROW and UNWIND-PROTECT work inside a coroutine"
         (equal (coroutine-transfer th nil) '("inside" :thrown :returned)))
  (check "and its cleanup ran" (equal *log* '(:cleaned)))
  (check "and main's own HANDLER-CASE around a transfer still works"
         (eq (handler-case (coroutine-transfer th nil) (error () :wrong)) :again)))

;; 6. Code EVAL runs in a coroutine (the interpreter) sees the coroutine's own
;; window: its HANDLER-CASE dispatches on the condition it signalled, not on
;; the last one MAIN handled.  The interpreter used to read window slots at
;; main's address, take the "main binds shallowly" fast path of a special read,
;; and see main's *CURRENT-CONDITION*: an ERROR clause bound main's old
;; condition and a SIMPLE-ERROR clause missed its own SIMPLE-ERROR.
(handler-case (car (eval 1)) (error () nil))   ; main's last handled condition: a TYPE-ERROR
(let ((ev (coroutine-create
           (lambda (v)
             (declare (ignore v))
             (list (eval '(handler-case (error "fresh") (error (e) (princ-to-string e))))
                   (handler-case
                       (eval '(handler-case (error 'simple-error :format-control "typed")
                               (simple-error (e) (princ-to-string e))))
                     (error () :escaped))
                   (eval '(let ((*probe* :bound-in-eval)) (list *probe*))))))))
  (check "EVAL'd code in a coroutine sees its own condition, its own clause, its own binding"
         (equal (coroutine-transfer ev nil) '("fresh" "typed" (:bound-in-eval)))))

(defun deep (n) (if (zerop n) 0 (+ 1 (deep (- n 1)))))
(let ((dp (coroutine-create (lambda (n) (deep n)))))
  (check "a coroutine recurses 100000 deep on its own stack" (eql (coroutine-transfer dp 100000) 100000)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
