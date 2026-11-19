;;;; dynb-wide-filter.lisp -- a worker's bound-key filter answers right with hundreds of bindings, and a
;;;; counter that saturates (one special bound 300 deep) never makes a read wrong.
;;;;
;;;; A hosted thread's filter is 4096 one-byte counters (mvm/prelude.lisp %DYNB-FILTER), sticky at 255.  It
;;;; used to be 256 16-bit counters: a page worker binding ~300 specials left a third of them zero, and two
;;;; reads in three of a special it never bound walked the whole stack (LENGTH 5x slower on such a thread).
;;;; Run: ./modus --script test/dynb-wide-filter.lisp  (modus-mac: MODUS_SLIDE=300000000)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (incf *checks*)
  (unless ok (incf *fails*))
  (format t "~&~:[FAIL~;ok  ~] ~a~%" ok name))

(defparameter *syms*
  (loop for i below 300
        collect (let ((s (intern (format nil "*WIDE-FILTER-~d*" i))))
                  (proclaim (list 'special s))
                  (setf (symbol-value s) (- i))
                  s)))
(defvar *deep* :global)
(defvar *free* :free)

(defun nest (n)
  "Bind *DEEP* N levels deep; at the bottom answer what each level reads."
  (if (zerop n)
      *deep*
      (let ((*deep* n))
        (let ((inner (nest (1- n))))
          (if (eql *deep* n) inner :lost)))))

(defun worker-body ()
  (let ((r '()))
    (progv *syms* (loop for i below 300 collect i)
      (push (every (lambda (s) (eql (symbol-value s) (position s *syms*))) *syms*) r)
      (push (eq *free* :free) r)
      (push (eql (nest 300) 1) r)            ; the innermost binding wins at every depth
      (push (eq *deep* :global) r)           ; and all 300 came off: the counter saturated, still right
      (push (eql (length (make-list 50)) 50) r))
    (push (every (lambda (s) (eql (symbol-value s) (- (position s *syms*)))) *syms*) r)
    (nreverse r)))

(let* ((ch (modus:make-channel)))
  (sb-thread:make-thread (lambda () (modus:channel-send ch (handler-case (worker-body) (error (e) (princ-to-string e))))))
  (let ((r (modus:channel-receive ch)))
    (if (stringp r)
        (check (format nil "worker ran (~a)" r) nil)
        (destructuring-bind (all-bound free deep-inner deep-after length-ok all-restored) r
          (check "300 bound specials each read their own value" all-bound)
          (check "an unbound special reads its global" free)
          (check "300-deep nesting of one special: innermost wins at each level" deep-inner)
          (check "after unwinding a saturated counter the global is read" deep-after)
          (check "library code under 300 bindings" length-ok)
          (check "after the PROGV every global is back" all-restored)))))
(check "main thread untouched" (and (eq *deep* :global) (eql (symbol-value (first *syms*)) 0)))
(format t "~&~d checks, ~d failed~%~:[DYNB-WIDE-FILTER FAIL~;DYNB-WIDE-FILTER PASS~]~%" *checks* *fails* (zerop *fails*))
