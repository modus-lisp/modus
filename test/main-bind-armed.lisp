;;;; main-bind-armed.lisp — THE MAIN THREAD'S LET OF A SPECIAL, WITH WORKERS
;;;; RUNNING, IS RIGHT ON BOTH SIDES AND NOT SLOW.
;;;;
;;;;   ./modus --script test/main-bind-armed.lisp
;;;;
;;;; Once any thread has been started (the threads-live word is set), the MAIN
;;;; thread still binds specials SHALLOWLY: it has no per-thread binding block,
;;;; so its LET writes the globals pair and its unbind writes the saved value
;;;; back -- and a worker reading a special it has NOT bound reads that pair,
;;;; i.e. sees main's current binding.  That is the semantics this file pins
;;;; (it is what test/hosted-dynbind.lisp and test/worker-special-read.lisp
;;;; already assume), while main and workers hammer the same specials at once:
;;;;
;;;;   MAIN'S OWN VIEW    inside main's LETs main reads exactly what it bound,
;;;;                      SETQ inside a binding hits the binding, a THROW or a
;;;;                      handled ERROR out of the middle restores, and after
;;;;                      the loops every global is back -- whatever workers did.
;;;;   A WORKER'S OWN     a worker binding the same specials reads only its own
;;;;                      values, many times over, while main rebinds them.
;;;;   A WORKER READING   a special the worker has NOT bound reads main's current
;;;;   MAIN'S             value: always one main could have installed (never
;;;;                      garbage, never another worker's), and a global main
;;;;                      SETQs at top level reads monotonically on the worker.
;;;;   THE INSERT PATH    specials with NO globals pair yet, first bound on main
;;;;                      after threads exist (a compiled LET of a DECLAIMed
;;;;                      special, and PROGV of freshly interned symbols), while
;;;;                      workers insert globals of their own: bound inside,
;;;;                      unbound again after.
;;;;
;;;; and one lenient timing ratio: a long loop of a 26-special LET on main
;;;; after threads armed stays within 3x of the same loop before any thread
;;;; (it was 20-100x: every bind and unbind took the runtime lock).
;;;;
;;;; Worker results are FIXNUM failure counts (a heap object returned through
;;;; JOIN-THREAD crosses regions by pointer; see test/hosted-dynbind.lisp).

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defvar *ma* :global-a)
(defvar *mb* :global-b)
(defvar *mc* :global-c)
(defvar *ctr* 0)
(defvar *stop* nil)

;;; Declared special, never given a value: NO globals pair until something
;;; binds it.  The first bind on main after threads exist is the insert path.
(declaim (special *late1* *late2*))
(defun read-late1 () *late1*)
(defun read-late2 () *late2*)

(defmacro def-many (prefix n)
  `(progn ,@(loop for i from 0 below n
                  collect `(defvar ,(intern (format nil "*~A~D*" prefix i)) 0))))
(def-many "MZ" 26)

(defmacro let-many ((prefix n val) &body body)
  `(let ,(loop for i from 0 below n
               collect `(,(intern (format nil "*~A~D*" prefix i)) ,val))
     ,@body))

(defmacro sum-many (prefix n)
  `(+ ,@(loop for i from 0 below n collect (intern (format nil "*~A~D*" prefix i)))))

(defun bind-loop (n)
  (let ((s 0))
    (dotimes (i n) (let-many ("MZ" 26 i) (setq s (logand (+ s (sum-many "MZ" 26)) #xffffff))))
    s))

(defun ms (f)
  (let ((t0 (get-internal-real-time)))
    (funcall f)
    (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))

(defun best-of (n f)
  (let ((b nil)) (dotimes (i n) (let ((m (ms f))) (when (or (null b) (< m b)) (setq b m)))) b))

;;; ---- the unarmed reference, before any thread exists ----
(bind-loop 1000)
(defvar *unarmed-ms* (best-of 3 (lambda () (bind-loop 40000))))

;;; ---- the main-side battery: returns a failure count ----
(defun read-ma () *ma*)
(defun read-mb () *mb*)

(defun main-battery (k)
  (let ((bad 0))
    (flet ((ck (ok) (unless ok (setq bad (+ bad 1)))))
      (let ((obj (list k)))
        (let ((*ma* k) (*mb* obj))
          (ck (eql *ma* k)) (ck (eq *mb* obj)) (ck (eql (read-ma) k)) (ck (eq (read-mb) obj))
          (let ((*ma* (+ k 1)))
            (ck (eql *ma* (+ k 1)))
            (setq *ma* (+ k 1000000))
            (ck (eql (read-ma) (+ k 1000000))))
          (ck (eql *ma* k))
          (setq *mb* k)
          (ck (eql (read-mb) k))))
      (ck (eq *ma* :global-a)) (ck (eq *mb* :global-b))
      ;; non-local exits through main's LETs
      (catch 'out
        (let ((*ma* k))
          (let ((*mb* k) (*mc* k))
            (ck (eql (read-mb) k))
            (throw 'out nil))))
      (ck (eq *ma* :global-a)) (ck (eq *mb* :global-b)) (ck (eq *mc* :global-c))
      (handler-case
          (let ((*ma* k) (*mc* (list k)))
            (let-many ("MZ" 26 k) (error "boom ~A" *mz3*)))
        (error () nil))
      (ck (eq *ma* :global-a)) (ck (eq *mc* :global-c)) (ck (eql *mz3* 0))
      (block b
        (let ((*mc* k)) (when (eql *mc* k) (return-from b nil))))
      (ck (eq *mc* :global-c)))
    bad))

;;; ---- the worker side ----
(defun main-value-p (v global)
  "A value main can have installed in *MA* / *MB*: the global, one of main's
   loop indices, its in-binding SETQ value, or a list main consed."
  (or (eq v global)
      (and (integerp v) (>= v 0) (< v 2000000))
      (and (consp v) (integerp (car v)))))

(defun worker-loop (base iters)
  "Bind the specials main is binding, read only our own; read the ones we do
   not bind and check they are main's.  Returns failures (a fixnum)."
  (let ((bad 0) (last-ctr 0) (i 0))
    (flet ((ck (ok) (unless ok (setq bad (+ bad 1)))))
      (loop
        (when (or (>= i iters) (and *stop* (>= i 2000))) (return nil))
        (let ((mine (+ base i)))
          (let ((*ma* mine))
            (ck (eql *ma* mine)) (ck (eql (read-ma) mine))
            ;; *MB* is NOT bound here: main's value (or the global)
            (ck (main-value-p (read-mb) :global-b))
            (let ((*mb* (- mine)))
              (ck (eql (read-mb) (- mine)))
              (setq *mb* (+ mine 7))
              (ck (eql *mb* (+ mine 7))))
            (ck (main-value-p *mb* :global-b))
            (catch 'wout (let ((*ma* :inner)) (throw 'wout nil)))
            (ck (eql *ma* mine))
            (when (= (mod i 64) 0)
              (let-many ("MZ" 26 mine) (ck (= (sum-many "MZ" 26) (* 26 mine))))))
          (ck (main-value-p *ma* :global-a))
          ;; a global main SETQs at top level: monotone here
          (let ((c *ctr*))
            (ck (and (integerp c) (>= c last-ctr)))
            (setq last-ctr c))
          ;; insert globals of our own, concurrently with main's inserts
          (when (= (mod i 500) 0)
            (setf (symbol-value (intern (format nil "MBA-W~D-~D" base i))) i)))
        (setq i (+ i 1))))
    bad))

(defun ctr-watcher ()
  "Read *CTR* (SETQ'd by main at top level) until it reaches 60: always an
   integer, never decreasing.  Main sets *STOP* only after its last SETQ, so a
   watcher that sees *STOP* and still reads under 60 scores 1000."
  (let ((bad 0) (last 0))
    (loop
      (let ((stop *stop*) (c *ctr*))
        (unless (and (integerp c) (>= c last)) (setq bad (+ bad 1)))
        (setq last c)
        (when (>= c 60) (return bad))
        (when stop (return (+ bad 1000))))
      ;; A SAFE POINT EVERY TURN.  A worker loop that only reads specials
      ;; (no allocation, no syscall) never lets a region-0 stop-the-world
      ;; collection finish: main and the other workers spin in the park
      ;; protocol and the process hangs -- on the base tree as well, so it
      ;; is not this file's subject.  Minimal form: a worker running
      ;; (loop (when *flag* (return))) while main conses.  The sleep is a
      ;; syscall, which is a safe point.
      (sleep 0.0002))))

(format t "~%=== MAIN'S LET OF A SPECIAL WITH WORKERS RUNNING ==========~%")
(format t "-- before any thread~%")
(chk "battery on main, unarmed" (main-battery 5) 0)

(format t "-- concurrent~%")
(defvar *w1* (sb-thread:make-thread (lambda () (worker-loop 10000 400000))))
(defvar *w2* (sb-thread:make-thread (lambda () (worker-loop 20000 400000))))
(defvar *w3* (sb-thread:make-thread (lambda () (ctr-watcher))))

;;; main's battery, many times, while the workers run
(defvar *main-bad* 0)
(dotimes (k 20000)
  (setq *main-bad* (+ *main-bad* (main-battery k))))
(chk "main's bindings, 20000 rounds against two binding workers" *main-bad* 0)

;;; THE INSERT PATH: specials with no pair, first bound on main now
(chk "*late1* has no value yet" (boundp '*late1*) nil)
(chk "a compiled LET of a declaimed special, first bound on main armed"
     (let ((*late1* 77)) (list *late1* (read-late1)))
     (list 77 77))
(chk "and it is unbound again after" (boundp '*late1*) nil)
(chk "LET* of two new ones, nested, with a throw"
     (catch 'lt
       (let* ((*late2* 5) (*late1* (+ *late2* 1)))
         (when (eql (read-late1) 6) (throw 'lt (read-late2)))
         :no))
     5)
(chk "both unbound again" (list (boundp '*late1*) (boundp '*late2*)) (list nil nil))
(defvar *progv-bad* 0)
(dotimes (i 300)
  (let ((s (intern (format nil "MBA-NEW-~D" i))))
    (progv (list s) (list (* i 3))
      (unless (eql (symbol-value s) (* i 3)) (incf *progv-bad*)))
    (when (boundp s) (incf *progv-bad*))))
(chk "300 freshly interned symbols PROGV-bound on main, then unbound" *progv-bad* 0)

;;; A GLOBAL MAIN SETQS AT TOP LEVEL, read by *W3*
(setq *ctr* 1)
(setq *ctr* 2)
(setq *ctr* 3)
(dotimes (i 57) (setq *ctr* (+ *ctr* 1)))
(chk "*ctr* on main" *ctr* 60)

(setq *stop* t)
(chk "worker 1: its own bindings, main's values" (sb-thread:join-thread *w1*) 0)
(chk "worker 2: its own bindings, main's values" (sb-thread:join-thread *w2*) 0)
(chk "worker 3: main's top-level SETQs, monotone" (sb-thread:join-thread *w3*) 0)

(chk "globals on main after everything"
     (list *ma* *mb* *mc* *mz0* *mz25*) (list :global-a :global-b :global-c 0 0))
(chk "and as a fresh worker reads them"
     (sb-thread:join-thread
      (sb-thread:make-thread
       (lambda () (if (and (eq *ma* :global-a) (eq *mb* :global-b) (eq *mc* :global-c)
                           (eql *mz0* 0) (eql *ctr* 60))
                      0 1))))
     0)
(chk "a worker's inserted global reads on main"
     (symbol-value (intern "MBA-W10000-0")) 0)
(chk "battery on main, armed, workers gone" (main-battery 7) 0)

(format t "-- timing~%")
(bind-loop 1000)
(let ((armed (best-of 3 (lambda () (bind-loop 40000)))))
  (format t "  26-special LET x40000 on main: unarmed ~D ms, armed ~D ms~%"
          *unarmed-ms* armed)
  (chk "armed main LET within 3x of unarmed" (<= armed (* 3 (max *unarmed-ms* 10))) t))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (= *fail* 0) "MAIN-BIND-ARMED: PASS" "MAIN-BIND-ARMED: FAIL"))
