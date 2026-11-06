;;;; thread-bridge-retire.lisp — a JIT'd call to a closure callee, made first on a WORKER.
;;;;
;;;;   ./modus --script test/thread-bridge-retire.lisp      -> BRIDGE-RETIRE PASS / FAIL
;;;;
;;;; A runtime DEFCLASS accessor is a closure, so a JIT'd caller reaches it through a bridge
;;;; thunk, and the first call RETIRES the call site: %JIT-BRIDGE-RESOLVE stored a fresh
;;;; (:RETIRED . site) cons into the process-global sites vector.  Made on a worker, that cons
;;;; is the worker's own object stored into shared memory, and the shared-store guard refused
;;;; it: "a thread stored one of its own objects into shared memory", raised inside the CALL.
;;;; glass's per-client sender thread died on exactly this at its first message, so
;;;; GLASS:SERVE on modus answered the handshake and never sent a pixel.
;;;;
;;;; Each caller below is fresh and never called on main, so its site is still pending when
;;;; the worker calls it.  A worker now leaves the site pending (it resolves through
;;;; %JIT-BRIDGE-RESOLVE each time); main still retires it.  Checked: the worker's answers, no
;;;; thread error, and main calling the same functions afterwards.

(defclass br-point () ((x :initarg :x :accessor br-x) (y :initarg :y :accessor br-y)))
(defvar *br-fail* 0)
(defun br-check (what got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "FAIL") what)
  (unless (equal got want)
    (format t "       got  ~s~%       want ~s~%" got want)
    (incf *br-fail*)))

;; Callers JIT'd at load (script DEFUNs are), never called on main before the worker runs.
(defun br-sum (p) (+ (br-x p) (br-y p)))
(defun br-scaled (p k) (* k (br-x p)))
(defun br-move (p dx) (setf (br-x p) (+ (br-x p) dx)) (br-x p))

(defun br-worker ()
  ;; The point is made HERE, on the worker: the setter's store is into the worker's own object.
  (let ((p (make-instance 'br-point :x 30 :y 12)))
    (list (br-sum p) (br-scaled p 3) (br-move p 5) (br-sum p))))

(let* ((th (sb-thread:make-thread #'br-worker :name "br-worker"))
       (r (handler-case (sb-thread:join-thread th)
            (error (e) (list :thread-error (princ-to-string e))))))
  (br-check "a worker calls accessors through fresh JIT'd callers" r '(42 90 35 47)))

;; Main afterwards: the same sites, still pending, retire as before and answer the same.
(let ((p (make-instance 'br-point :x 30 :y 12)))
  (br-check "main, after the worker" (list (br-sum p) (br-scaled p 3) (br-move p 5) (br-sum p))
            '(42 90 35 47)))

(format t "~&BRIDGE-RETIRE ~a~%" (if (zerop *br-fail*) "PASS" "FAIL"))
