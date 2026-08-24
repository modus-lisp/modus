;;;; hosted-worker-intern.lisp — A WORKER THREAD THAT INTERNS FRESH SYMBOLS DIES.
;;;;
;;;;   test/run-worker-intern.sh [MODUS-BINARY]
;;;;   WORKER_ARM=cons|format|intern-same|intern-fresh ./modus --script <this>
;;;;
;;;; ============================================================
;;;; WHAT THIS IS
;;;; ============================================================
;;;;
;;;; The smallest thing found so far that reproduces the failure standing
;;;; between modus and glass's RFB server, with no glass, no sockets, no
;;;; recursive lock and no special rebinding in it: ONE worker thread, in a
;;;; loop, interning symbols.
;;;;
;;;; It is SELF-BISECTING.  Four arms, each a loop of the same length on the
;;;; same thread, differing only in what the loop body does:
;;;;
;;;;   cons          conses — no lock, no shared table
;;;;   format        FORMAT NIL — allocates strings, touches no shared table
;;;;   intern-same   INTERN of ONE name, repeatedly — takes the runtime lock
;;;;                 and searches the shared table, but allocates no new symbol
;;;;   intern-fresh  INTERN of a NEW name each time — takes the lock AND adds
;;;;                 a symbol to the shared table
;;;;
;;;; MEASURED: the first three are clean.  `intern-fresh' dies, and it dies
;;;; with the same two signatures the glass sender dies with —
;;;; `MVM LONGJMP (TRAP #x0511) with no active handler-case' and a bare
;;;; TYPE-ERROR whose condition object carries no useful slots.
;;;;
;;;; SO IT IS NOT: consing on a worker, allocating on a worker, taking the
;;;; runtime lock on a worker, or searching the shared tables from a worker.
;;;; What is left is ADDING to them.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE ARMS SHARE ONE LOOP AND ONE THREAD.  They differ by their body and
;;;;   by nothing else, so a difference between them is the body.
;;;;
;;;;   THE MAIN THREAD IS NOT INVOLVED, AND THAT IS CHECKED RATHER THAN
;;;;   ASSUMED.  MAIN_ROUNDS controls how much main allocates concurrently;
;;;;   at 0 it allocates nothing at all after spawning, and `intern-fresh'
;;;;   still dies.  So this is not two threads racing for one heap.
;;;;
;;;;   IT IS LAYOUT-SENSITIVE, LIKE EVERYTHING ELSE IN THIS CAMPAIGN.  Whether
;;;;   a given arm dies moves with the length of the symbol names and with
;;;;   whether the loop reads a global.  A single passing run is therefore not
;;;;   evidence of health — read the RATE, and treat a clean arm as "not
;;;;   reproduced in this shape" rather than "correct".
;;;;
;;;;   A CHAIN IS HELD ACROSS THE WHOLE RUN and walked at the end, so a run
;;;;   that completes having quietly corrupted the main thread's heap is a
;;;;   failure and not a pass.

(defvar *k*
  (let ((s (%cli-getenv "WORKER_K")))
    (if (and s (> (length s) 0)) (parse-integer s) 6000)))

(defvar *main-rounds*
  (let ((s (%cli-getenv "MAIN_ROUNDS")))
    (if (and s (> (length s) 0)) (parse-integer s) 0)))

(defvar *arm*
  (let ((s (%cli-getenv "WORKER_ARM")))
    (if (and s (> (length s) 0)) s "intern-fresh")))

(defvar *chain-n* 20000)

(defun mkchain (n)
  (let ((c nil) (i 0))
    (loop (when (>= i n) (return nil))
      (setq c (cons i c))
      (setq i (+ i 1)))
    c))

(defun chain-ok (c n)
  "Every car must equal its index counting DOWN from n-1."
  (let ((want (- n 1)) (ok 1))
    (loop
      (when (null c) (return nil))
      (unless (eql (car c) want) (setq ok 0) (return nil))
      (setq want (- want 1))
      (setq c (cdr c)))
    (if (and (= ok 1) (= want -1)) 1 0)))

(defun w-cons (k)
  (let ((i 0) (c nil))
    (loop (when (>= i k) (return nil))
      (setq c (cons i c))
      (setq i (+ i 1)))
    (if c k 0)))

(defun w-format (k)
  (let ((i 0) (last nil))
    (loop (when (>= i k) (return nil))
      (setq last (format nil "WI-~D" i))
      (setq i (+ i 1)))
    (if last k 0)))

(defun w-intern-same (k)
  (let ((i 0))
    (loop (when (>= i k) (return nil))
      (intern "WI-ONE-FIXED-NAME" "COMMON-LISP-USER")
      (setq i (+ i 1)))
    k))

(defun w-intern-fresh (k)
  (let ((i 0))
    (loop (when (>= i k) (return nil))
      (intern (format nil "WI-FRESH-~D" i) "COMMON-LISP-USER")
      (setq i (+ i 1)))
    k))

(let* ((arm *arm*)
       (n *chain-n*)
       (chain (mkchain n))
       (body (cond ((string= arm "cons")        (lambda () (w-cons *k*)))
                   ((string= arm "format")      (lambda () (w-format *k*)))
                   ((string= arm "intern-same") (lambda () (w-intern-same *k*)))
                   (t                           (lambda () (w-intern-fresh *k*)))))
       (th (sb-thread:make-thread body :name "wi-worker")))
  ;; MAIN_ROUNDS=0 means main allocates nothing at all while the worker runs,
  ;; which is how "this is not a two-thread heap race" is established.
  (let ((r 0) (rounds *main-rounds*))
    (loop
      (when (>= r rounds) (return nil))
      (mkchain 4000)
      (setq r (+ r 1))))
  (let ((got (sb-thread:join-thread th))
        (intact (chain-ok chain n)))
    (format t "~&arm=~a k=~d main-rounds=~d worker=~s chain-intact=~d~%"
            arm *k* *main-rounds* got intact)
    (finish-output)
    (if (and got (= intact 1))
        (progn (format t "ARM ~a: CLEAN~%" arm) (finish-output) (sys-exit 0))
        (progn (format t "ARM ~a: FAILED~%" arm) (finish-output) (sys-exit 1)))))
