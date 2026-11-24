;;;; worker-dict-pair.lisp -- a worker's first call through the symbol-function table
;;;; makes that name's pair in SHARED memory (mvm/prelude.lisp, A WORKER MAKES A SHARED
;;;; TABLE'S PAIR UNDER THE RUNTIME LOCK).
;;;;   modus --script test/worker-dict-pair.lisp
;;;; Before: on hosted x86-64 every MAKE-THREAD body died before its first form --
;;;; "a thread stored one of its own objects into shared memory ... (at PUTHASH)" --
;;;; and JOIN-THREAD returned NIL.  (AArch64 passed before too: its workers made no
;;;; pair of their own here.)
(defvar *fail* 0)
(defmacro check (name form) `(unless ,form (incf *fail*) (format t "~&FAIL: ~a~%" ,name)))
(defun run (fn) (sb-thread:join-thread (sb-thread:make-thread fn)))

(check "a thread returns its value" (eq (run (lambda () :ok)) :ok))
(check "a thread returns a list it made" (equal (run (lambda () (list 1 2))) '(1 2)))
(check "the thread after that too" (eql (run (lambda () 42)) 42))

;; A name nothing has called yet, called first on a worker, by symbol.
(defun %wdp-fresh (x) (* x 3))
(check "a worker's first call of a fresh function by name" (eql (run (lambda () (funcall '%wdp-fresh 5))) 15))
(check "and main's call after it" (eql (funcall '%wdp-fresh 2) 6))

;; The pairs it made are not its own objects: they outlive it, in a shared table.
;; Counted on the worker, over the symbol-function table's side, after its calls.
(defun %wdp-fresh-2 (x) (+ x 1))
(defun %wdp-own-pairs ()
  (let ((side (modus-int::%ht-pair-side modus-int::*symbol-function-table* nil)) (n 0) (all 0))
    (when side
      (maphash (lambda (k p) k (incf all) (when (modus-int::%own-region-object-p p) (incf n))) side))
    (+ (* n 1000000) all)))
(let ((r (run (lambda () (funcall '%wdp-fresh-2 1) (%wdp-own-pairs)))))
  (format t "~&side pairs ~a, the worker's own ~a~%" (mod r 1000000) (floor r 1000000))
  (check "the side has pairs" (> (mod r 1000000) 0))
  (check "none of them is the worker's own object" (zerop (floor r 1000000))))

(check "a worker EVALs a form it made" (eql (run (lambda () (eval (list '+ 1 2)))) 3))
(check "and a package-qualified call through EVAL" (eql (run (lambda () (eval (list 'cl-user::%wdp-fresh 2)))) 6))
(sb-ext:gc)
(check "redefinition reaches the pair a worker made"
       (progn (defun %wdp-fresh-2 (x) (+ x 100)) (eql (run (lambda () (funcall '%wdp-fresh-2 1))) 101)))
(format t "~&WORKER-DICT-PAIR ~a~%" (if (zerop *fail*) "PASS" (format nil "FAIL (~a)" *fail*)))
