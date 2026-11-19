;;;; worker-special-read.lisp — READING A BOUND SPECIAL ON A WORKER IS RIGHT,
;;;; AND NOT SLOW.
;;;;
;;;;   ./modus --script test/worker-special-read.lisp
;;;;
;;;; A worker's dynamic bindings live on that thread's own binding stack
;;;; (PER-THREAD DYNAMIC BINDINGS, mvm/prelude.lisp), and a read of a special
;;;; the worker has bound used to WALK that stack down to the binding.  Code
;;;; that binds many specials and reads them in its inner loops
;;;; (secp256k1-fast's WITH-FRESH-CT-SCRATCH: ~55 per call) paid that walk on
;;;; every read, and ran 2.5-3.5x slower on a worker than on main.  The read
;;;; now goes through a per-thread INDEX CACHE (%DYNB-LOOKUP), which is exactly
;;;; the kind of thing that is fast and wrong when it is stale, so most of
;;;; this file is the correctness questions a stale cache would get wrong:
;;;;
;;;;   ISOLATION     two workers binding the same specials concurrently each
;;;;                 read only their own, many times over; main never sees
;;;;                 either.
;;;;   MANY          60 specials bound at once (more than enough to collide in
;;;;                 the cache's 512 slots... and in the filter's 256), every
;;;;                 one read back.
;;;;   REBIND        inner binding shadows, the outer comes back, and a
;;;;                 DIFFERENT special bound at the slot the inner one used
;;;;                 does not resurrect it.
;;;;   UNWIND        THROW and a handled ERROR out of a binding restore it.
;;;;   GLOBALS       a value SETQ'd on main before the worker starts reads
;;;;                 correctly there, bound or not around it.
;;;;
;;;; and then one timing ratio, generous enough not to flake: a worker reading
;;;; specials bound under 60 others must stay within 3x of main doing the same
;;;; (it was 5-10x).

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defmacro def-many (prefix n)
  `(progn ,@(loop for i from 0 below n
                  collect `(defvar ,(intern (format nil "*~A~D*" prefix i)) ,(- i)))))
(def-many "WS" 60)

(defmacro with-many ((prefix n base) &body body)
  "Bind *PREFIX0*..*PREFIX(N-1)* to BASE+i."
  `(let ,(loop for i from 0 below n
               collect `(,(intern (format nil "*~A~D*" prefix i)) (+ ,base ,i)))
     ,@body))

(defmacro sum-many (prefix n)
  `(+ ,@(loop for i from 0 below n collect (intern (format nil "*~A~D*" prefix i)))))

(defvar *x* :global-x)
(defvar *y* :global-y)
(defvar *g* 0)

(defun battery (tag base)
  "Every correctness question; returns the number of failures (a fixnum, so
   it crosses JOIN-THREAD without a cross-region pointer)."
  (let ((bad 0))
    (flet ((ck (ok) (unless ok (setq bad (+ bad 1)))))
      ;; MANY: 60 bound at once, each read back; and the unbound ones (none)
      (with-many ("WS" 60 base)
        (ck (= (sum-many "WS" 60) (+ (* 60 base) (/ (* 59 60) 2))))
        (ck (= *ws0* base))
        (ck (= *ws59* (+ base 59)))
        ;; read them all many times: the cache hit path
        (let ((s 0))
          (dotimes (i 2000) (setq s (+ s *ws0* *ws17* *ws33* *ws59*)))
          (ck (= s (* 2000 (+ (* 4 base) 0 17 33 59))))))
      (ck (= *ws0* 0))
      (ck (= *ws59* -59))
      ;; REBIND
      (let ((*x* 1))
        (ck (eql *x* 1))
        (let ((*x* 2))
          (ck (eql *x* 2))
          (let ((*x* 3)) (ck (eql *x* 3)))
          (ck (eql *x* 2)))
        (ck (eql *x* 1))
        ;; a different special at the entry the inner *x* used
        (let ((*y* 9))
          (ck (eql *y* 9))
          (ck (eql *x* 1)))
        (ck (eql *x* 1))
        (setq *x* 5)
        (ck (eql *x* 5)))
      (ck (eq *x* :global-x))
      ;; UNWIND: throw
      (let ((*x* 10))
        (catch 'out
          (let ((*x* 11) (*y* 12))
            (ck (eql *x* 11))
            (throw 'out nil)))
        (ck (eql *x* 10))
        (ck (eq *y* :global-y)))
      ;; UNWIND: error
      (let ((*x* 20))
        (handler-case
            (let ((*x* 21))
              (ck (eql *x* 21))
              (with-many ("WS" 60 1000) (error "boom ~A" *ws3*)))
          (error () nil))
        (ck (eql *x* 20))
        (ck (eql *ws3* -3)))
      (ck (eq *x* :global-x))
      ;; GLOBALS
      (ck (eql *g* 4242))
      (let ((*x* 1)) (ck (eql *g* 4242)))
      (with-many ("WS" 60 0) (ck (eql *g* 4242))))
    (unless (= bad 0) (format t "  ~A: ~D failures~%" tag bad))
    bad))

(setq *g* 4242)

(format t "~&-- correctness~%")
(chk "battery on main (before any thread)" (battery "main0" 100) 0)
(chk "battery on a worker"
     (sb-thread:join-thread (sb-thread:make-thread (lambda () (battery "worker" 200))))
     0)
(chk "battery on main (threads armed)" (battery "main1" 300) 0)

;;; ISOLATION: two workers, the same specials, concurrently.
(defun isolate (base)
  (let ((bad 0))
    (with-many ("WS" 60 base)
      (let ((*x* base))
        (dotimes (i 20000)
          (unless (and (eql *x* base) (eql *ws0* base) (eql *ws31* (+ base 31))
                       (eql *ws59* (+ base 59)))
            (setq bad (+ bad 1)))
          (when (= (mod i 1000) 0)
            ;; rebind in the middle so the stacks keep moving
            (let ((*x* (+ base 1)) (*ws31* 7))
              (unless (and (eql *x* (+ base 1)) (eql *ws31* 7)) (setq bad (+ bad 1))))))))
    bad))

(let ((a (sb-thread:make-thread (lambda () (isolate 1000))))
      (b (sb-thread:make-thread (lambda () (isolate 5000)))))
  (let ((main-bad 0))
    (dotimes (i 20000)
      (unless (and (eq *x* :global-x) (eql *ws0* 0) (eql *ws59* -59))
        (setq main-bad (+ main-bad 1))))
    (chk "isolation: worker A" (sb-thread:join-thread a) 0)
    (chk "isolation: worker B" (sb-thread:join-thread b) 0)
    (chk "isolation: main never saw a worker's binding" main-bad 0)))
(chk "after the workers, globals are untouched"
     (list *x* *ws0* *ws59* *g*) (list :global-x 0 -59 4242))

;;; ---- timing ----
(defun ms (f)
  (let ((t0 (get-internal-real-time)))
    (funcall f)
    (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))

(defun hot ()
  "Read specials bound UNDER 60 others -- the shape that walked the stack."
  (let ((*x* 1) (*y* 2))
    (with-many ("WS" 60 0)
      (let ((s 0))
        (dotimes (i 400000)
          (setq s (logand (+ s *x* *y* *ws0* *ws1*) #xffff)))
        s))))

(defun best-of (n f) (let ((b nil)) (dotimes (i n) (let ((m (ms f))) (when (or (null b) (< m b)) (setq b m)))) b))

(format t "~&-- timing~%")
(hot)
(let* ((main (best-of 3 #'hot))
       (worker (sb-thread:join-thread (sb-thread:make-thread (lambda () (hot) (best-of 3 #'hot))))))
  (format t "  main ~D ms, worker ~D ms~%" main worker)
  (chk "worker special reads within 3x of main" (<= worker (* 3 (max main 5))) t))

(format t "~&~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (= *fail* 0) "WORKER-SPECIAL-READ: PASS" "WORKER-SPECIAL-READ: FAIL"))
