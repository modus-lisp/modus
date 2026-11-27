;;;; thread-bindings.lisp -- dynamic bindings across threads, as SBCL has them.
;;;;
;;;;   test/run-thread-bindings.sh [MODUS]     (runs this under SBCL and modus
;;;;                                           and requires identical output)
;;;;
;;;; A binding belongs to the thread that made it: a new thread sees the GLOBAL
;;;; value of every special, whatever its creator has bound (CLHS 3.1.2.1.1.2
;;;; says nothing of threads; SBCL, CCL, ECL and LispWorks all agree).  On modus
;;;; the MAIN thread used to bind shallowly -- into the globals cell every thread
;;;; reads -- so a worker saw main's bindings (mvm/prelude.lisp, THE MAIN
;;;; THREAD'S BINDING STACK).  Every case prints one line; nothing here depends
;;;; on scheduling except where a loop counts disagreements, which must be 0.

(defvar *x* :global)
(defvar *y* :global-y)
(defvar *z* 0)
(defvar *u*)                            ; no global value
(defvar *rt0* *readtable*)
(defvar *w* :global-w)

(defun churn () (dotimes (i 3) (sb-ext:gc :full t)))

(defun on-worker (fn)
  (sb-thread:join-thread (sb-thread:make-thread fn) :default :died))

(defun show (name v) (format t "~a: ~s~%" name v))

;;; 1. Bindings main holds BEFORE the first thread exists.  This must be the
;;; first thread the file starts: a LOAD-time setq of *READTABLE* (LOAD binds
;;; it), an outer LET with a SETQ inside, and an inner LET of the same
;;; variable around the very first MAKE-THREAD.
(setq *readtable* (copy-readtable nil))
(let ((*x* :outer))
  (setq *x* :outer-setq)
  (let ((*x* :inner) (*y* :inner-y) (*w* (list :a (list :b))))
    ;; heap values bound before threads exist, across collections on both
    ;; sides of the first MAKE-THREAD
    (let ((held *w*))
      (churn)
      (show "first-thread sees *w*" (on-worker (lambda () *w*)))
      (churn)
      (show "main's *w* after collections" (list (eq held *w*) *w*)))
    (show "first-thread sees *x*" (on-worker (lambda () *x*)))
    (show "first-thread sees *y*" (on-worker (lambda () *y*)))
    (show "first-thread readtable is the global one" (on-worker (lambda () (eq *readtable* *rt0*))))
    (show "main still sees *x*" *x*))
  (show "main after inner LET" *x*))
(show "main after outer LET" *x*)
(show "main readtable still its own" (not (eq *readtable* *rt0*)))

;;; 2. LET / LET* / PROGV on main, now that threads exist.
(let ((*x* :let))
  (show "LET: worker" (on-worker (lambda () *x*))))
(let* ((*x* :let*) (*y* (list *x*)))
  (show "LET*: worker" (on-worker (lambda () (list *x* *y*))))
  (show "LET*: main" (list *x* *y*)))
(progv '(*x* *y*) '(:progv-x :progv-y)
  (show "PROGV: worker" (on-worker (lambda () (list *x* *y*))))
  (show "PROGV: main" (list *x* *y*)))
(show "after PROGV main" (list *x* *y*))

;;; 3. SETQ inside a binding hits the binding, not the global.
(let ((*x* :b))
  (setq *x* :b-setq)
  (show "SETQ in binding: worker" (on-worker (lambda () *x*)))
  (show "SETQ in binding: main" *x*))
(show "SETQ in binding: after" *x*)

;;; 4. A worker's SETQ of the global while main is inside a binding: main
;;; keeps its binding, and sees the worker's value once the binding ends.
(let ((*z* 100))
  (on-worker (lambda () (setq *z* 7)))
  (show "worker SETQ global: main inside" *z*))
(show "worker SETQ global: main after" *z*)
(setq *z* 0)

;;; 5. Non-local exits out of main's bindings.
(catch 'out
  (let ((*x* :thrown))
    (throw 'out nil)))
(show "after THROW: main" *x*)
(show "after THROW: worker" (on-worker (lambda () *x*)))
(block b
  (let ((*x* :returned))
    (return-from b nil)))
(show "after RETURN-FROM: main" *x*)
(handler-case (let ((*x* :errored)) (error "boom"))
  (error () nil))
(show "after ERROR: main" *x*)
(show "after ERROR: worker" (on-worker (lambda () *x*)))

;;; 6. Nested bindings on main; a worker binds the same variable.
(let ((*x* 1))
  (let ((*x* 2))
    (let ((*x* 3))
      (show "nested: worker" (on-worker (lambda () *x*)))
      (show "nested: worker binds" (on-worker (lambda () (let ((*x* :w)) (list *x* (let ((*x* :w2)) *x*) *x*)))))
      (show "nested: main" *x*))
    (show "nested: main 2" *x*))
  (show "nested: main 1" *x*))

;;; 7. SYMBOL-VALUE, BOUNDP, (SETF SYMBOL-VALUE).
(let ((*u* :bound-on-main))
  (show "BOUNDP *u* main" (boundp '*u*))
  (show "BOUNDP *u* worker" (on-worker (lambda () (boundp '*u*))))
  (show "SYMBOL-VALUE *x* worker" (let ((*x* :sv)) (on-worker (lambda () (symbol-value '*x*)))))
  (setf (symbol-value '*u*) :set-on-main)
  (show "SETF SYMBOL-VALUE main" (symbol-value '*u*)))
(show "BOUNDP *u* after" (boundp '*u*))

;;; 8. Printer variables: a worker formats with the global *PRINT-BASE*.
(let ((*print-base* 16))
  (show "*print-base* 16, worker formats 255"
        (on-worker (lambda () (format nil "~a" 255)))))

;;; 9. A worker is given main's value only explicitly.
(let ((*x* :explicit))
  (let ((v *x*))
    (show "explicit pass" (on-worker (lambda () (let ((*x* v)) *x*))))))

;;; 10. Heap values main binds now, across collections.
(let ((*w* (list 1 (list 2 3) "four")))
  (let ((held *w*))
    (churn)
    (show "armed main's *w* after collections" (list (eq held *w*) *w*))
    (show "worker sees *w*" (on-worker (lambda () *w*)))))

;;; 11. Main rebinds in a loop while a worker reads: the worker only ever
;;; sees the global; and the worker rebinds while main reads.
(let* ((stop nil)
       (bad 0)
       (th (sb-thread:make-thread
            (lambda ()
              (let ((n 0) (b 0))
                (loop until stop
                      do (incf n)
                         (unless (eq *x* :global) (incf b))
                         (let ((*y* n)) (unless (eql *y* n) (incf b))))
                b)))))
  (dotimes (i 20000)
    (let ((*x* i) (*y* :main))
      (unless (and (eql *x* i) (eq *y* :main)) (incf bad))))
  (setq stop t)
  (show "concurrent: worker saw a binding" (sb-thread:join-thread th :default :died))
  (show "concurrent: main lost its own" bad))
(show "end: *x* *y* *z*" (list *x* *y* *z*))
