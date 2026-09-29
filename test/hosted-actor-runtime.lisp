;;;; hosted-actor-runtime.lisp — acceptance for net/hosted-actor-runtime.lisp.
;;;;
;;;;   ./modus --script test/hosted-actor-runtime.lisp
;;;;
;;;; The runtime a program uses (docs/hosted-actor-runtime.md): actors scheduled
;;;; M:N over native threads, blocking receive, messages by copy, per-actor
;;;; windows, and region-0 collections that reach actors.  Each check names the
;;;; gap it closes.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *shared* nil)
(defvar *g* nil)
(defvar *dyn* :global)
(defstruct pt x y)
(defclass cc () ((a :initarg :a :accessor cc-a)))

(defun chk (name ok &optional detail)
  (setq *checks* (+ *checks* 1))
  (if ok
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A ~S~%" name detail)))
  (finish-output))

(defun echo ()
  (loop (let ((m (actors-receive)))
          (when (eq m :stop) (return 0))
          (actors-send (first m) (second m)))))

(defun relay ()
  (let ((next (second (actors-receive))))
    (loop (let ((m (actors-receive)))
            (cond ((eq m :stop) (return 0))
                  ((>= (second m) 300) (actors-send 1 (list :done (second m))))
                  (t (actors-send next (list :tok (+ 1 (second m))))))))))

(defun ctx-actor ()
  (let ((*dyn* (list :mine (actors-self))))
    (handler-case
        (let ((n 0))
          (loop (let ((m (actors-receive)))
                  (when (eq m :stop) (return 0))
                  (when (eq m :boom) (error "boom in ~a" (actors-self)))
                  (incf n)
                  (actors-send 1 (list :ctx (actors-self) (second *dyn*))))))
      (error (e) (actors-send 1 (list :caught (actors-self) (second *dyn*)))))))

(defun crasher () (actors-receive) (car 5))
(defun quick () (actors-send 1 (list :quick (actors-self))))
(defun leaker ()
  (actors-receive)
  (actors-send 1 (handler-case (progn (setq *g* (list 1)) :stored) (error (e) :refused))))

(defun holder ()
  (let ((mine *shared*))
    (loop (let ((m (actors-receive)))
            (when (eq m :stop) (return 0))
            (actors-send 1 (list (length mine) (first mine) (car (last mine))))))))

(defun busy ()
  (let ((ref *shared*) (bad 0) (iters 0) (keep nil))
    (loop
      (setq keep (list ref (list ref (length ref)) keep))
      (when (> (length keep) 3) (setq keep (list ref)))
      (unless (and (= (length ref) 2000) (= (first ref) 0) (= (car (last ref)) 5997))
        (incf bad))
      (incf iters)
      (when (zerop (mod iters 2000)) (actors-yield))
      (when (>= iters 40000) (return 0)))
    (actors-send 1 bad)))

(defun waiter ()
  ;; (:wait ms): report whether a receive with that timeout got a message, and how long it took
  (loop (let ((m (actors-receive)))
          (when (eq m :stop) (return 0))
          (let ((t0 (get-internal-real-time)))
            (multiple-value-bind (msg got) (actors-receive (second m))
              (actors-send 1 (list :waited got msg
                                   (round (* 1000 (- (get-internal-real-time) t0))
                                          internal-time-units-per-second))))))))

(defun selector ()
  (actors-receive)
  (let ((r (actors-receive-if (lambda (m) (and (consp m) (eq (first m) :reply))) 2000)))
    (actors-send 1 (list :selected r (actors-receive 500) (actors-receive 500)))))

(defun churn-main (n)
  (let ((keep nil)) (dotimes (i n) (setq keep (make-list 1000 :initial-element i))) (length keep)))

(jit-eager)
(format t "~%=== hosted actor runtime ===~%")
(chk "start: 3 scheduler threads, 12 slots" (actors-start 3 12 #x400000))

;; MESSAGES (gap 5): every data type round-trips by copy; a function is refused.
(let ((a (actors-spawn 'echo)))
  (dolist (v (list 0 nil t -5 (- (expt 2 61)) 123456789012345678901234567890 1.5d0 1/3 #\x
                   "héllo" :kw 'foo (list 1 (list 2 "three") (cons 4 5)) (vector 1 "v" :k)))
    (actors-send a (list 1 v))
    (let ((r (actors-receive)))
      (chk (format nil "message ~S" (type-of v)) (equalp r v) r)))
  (actors-send a (list 1 (make-pt :x 1 :y (list 2))))
  (let ((r (actors-receive))) (chk "message struct" (and (typep r 'pt) (equal (pt-y r) (list 2))) r))
  (actors-send a (list 1 (make-instance 'cc :a "inst")))
  (let ((r (actors-receive))) (chk "message CLOS instance" (and (typep r 'cc) (equal (cc-a r) "inst")) r))
  (let ((h (make-hash-table :test 'equal))) (setf (gethash "k" h) 42)
    (actors-send a (list 1 h))
    (let ((r (actors-receive))) (chk "message hash table" (and (hash-table-p r) (eql (gethash "k" r) 42)))))
  (chk "a closure is refused"
       (handler-case (progn (actors-send a (lambda () 1)) nil) (error (e) t)))
  (actors-send a :stop))

;; M:N: a ring of five over three threads.
(let ((ids (loop repeat 5 collect (actors-spawn 'relay))))
  (loop for (a b) on ids do (actors-send a (list :next (or b (first ids)))))
  (actors-send (first ids) (list :tok 0))
  (chk "ring of 5 actors, 300 hops" (equal (actors-receive) (list :done 300)))
  (dolist (a ids) (actors-send a :stop)))

;; PER-ACTOR WINDOWS (gap 4): a special binding and a HANDLER-CASE held
;; across blocking receives, two actors interleaving.
(let ((a (actors-spawn 'ctx-actor)) (b (actors-spawn 'ctx-actor)) (ok t))
  (dotimes (i 3)
    (actors-send a :go) (actors-send b :go)
    (dotimes (j 2) (let ((r (actors-receive))) (unless (eql (second r) (third r)) (setq ok nil)))))
  (chk "each actor sees its own binding across receives" ok)
  (actors-send a :boom)
  (let ((r (actors-receive)))
    (chk "its own HANDLER-CASE catches its error after interleaving"
         (and (eq (first r) :caught) (eql (second r) a) (eql (third r) a)) r))
  (actors-send b :go)
  (chk "the other actor is unaffected" (eql (second (actors-receive)) b))
  (actors-send b :stop))

;; SUPERVISION (first cut): an unhandled error ends only its actor.
(let ((c (actors-spawn 'crasher)) (q (actors-spawn 'quick)))
  (actors-send c :go)
  (chk "a crashing actor does not stop the others" (eq (first (actors-receive)) :quick)))

;; THE SHARED-STORE GUARD SEES ACTORS (gap 3).
(let ((l (actors-spawn 'leaker)))
  (actors-send l :go)
  (chk "an actor storing its own object into a global is refused"
       (and (eq (actors-receive) :refused) (null *g*))))

;; SLOT REUSE (gap 1).
(chk "40 spawns through 12 slots"
     (= 40 (let ((n 0)) (dotimes (i 40) (actors-spawn 'quick) (actors-receive) (incf n)) n)))

;; STOP-THE-WORLD REACHES ACTORS (gap 2): parked, then running.
(setq *shared* (loop for i below 2000 collect (* i 3)))
(let ((a (actors-spawn 'holder)) (ok t))
  (dotimes (round 3)
    (churn-main 40000)
    (actors-send a :go)
    (unless (equal (actors-receive) (list 2000 0 5997)) (setq ok nil)))
  (chk "a parked actor's region-0 reference survives region-0 collections" ok)
  (actors-send a :stop))
(let ((c0 (%gc-meta-read (+ (%gc-region-0) #x20) (%gc-meta-scale))))
  (dotimes (i 4) (actors-spawn 'busy))
  (churn-main 60000)
  (let ((bad (loop repeat 4 sum (actors-receive)))
        (ncoll (- (%gc-meta-read (+ (%gc-region-0) #x20) (%gc-meta-scale)) c0)))
    (chk "running actors' region-0 references survive region-0 collections"
         (and (zerop bad) (> ncoll 0)) (list :bad bad :collections ncoll))))

;; TIMEOUTS and SELECTIVE RECEIVE.
(let ((w (actors-spawn 'waiter)))
  (actors-send w (list :wait 150))
  (let ((r (actors-receive)))
    (chk "an actor's receive times out" (and (null (second r)) (>= (fourth r) 140) (< (fourth r) 1500)) r))
  (actors-send w (list :wait 3000))
  (sleep 0.05)
  (actors-send w :hello)
  (let ((r (actors-receive)))
    (chk "a message before the timeout wins" (and (second r) (eq (third r) :hello) (< (fourth r) 2000)) r))
  (actors-send w :stop))
(let ((t0 (get-internal-real-time)))
  (multiple-value-bind (m got) (actors-receive 120)
    (let ((ms (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
      (chk "main's receive times out" (and (null got) (null m) (>= ms 110) (< ms 1500)) ms))))
(let ((s (actors-spawn 'selector)))
  (actors-send s :go)
  (actors-send s (list :other 1)) (actors-send s (list :reply 7)) (actors-send s (list :other 2))
  (let ((r (actors-receive)))
    (chk "selective receive takes the reply and keeps the rest in order"
         (equal r (list :selected (list :reply 7) (list :other 1) (list :other 2))) r)))
(let ((ws (loop repeat 3 collect (actors-spawn 'waiter))) (t0 (get-internal-real-time)))
  (dolist (w ws) (actors-send w (list :wait 300)))
  (dotimes (i 3) (actors-receive))
  (let ((ms (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
    (chk "waiting actors do not hold a thread (3 x 300 ms on 3 threads < 900 ms)" (< ms 850) ms))
  (dolist (w ws) (actors-send w :stop)))

(chk "stop" (actors-stop))

(format t "~%=== VERDICT ==============================================~%")
(format t "HOSTED ACTOR RUNTIME: ~A (~D checks)~%"
        (if (zerop *fail*) "PASS" (format nil "FAIL (~D of them)" *fail*)) *checks*)
