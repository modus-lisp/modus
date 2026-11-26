;;;; ppcmac-threads.lisp -- cooperative threads on the bare PowerPC Mac.
;;;;
;;;; One CPU, no interrupts: a thread runs until it calls CO-YIELD (or a wait
;;;; that does: the net stack's YIELD and IO-DELAY do, once threads are on), and
;;;; the next live thread, round robin, resumes where it last yielded.  The
;;;; glass desk is thread 0 (the boot stack); the SSH server is the first one
;;;; spawned (scripts/ppcmac/ppcmac-desk.lisp).
;;;;
;;;; THE MACHINE UNDER IT (translate-ppc.lisp, #x0560 / #x0561, and the
;;;; collector's PARKED THREADS):
;;;;   %CO-SWITCH from to   saves this computation in save area FROM -- the
;;;;                        registers translated code keeps across a call, a
;;;;                        resume address, and the handler-case/catch state
;;;;                        [#x10000180, #x100009E0) -- and resumes TO's.
;;;;   %CO-FORK area top    saves this one in AREA (it resumes with 1) and goes
;;;;                        on below TOP with an empty handler stack (0).
;;;; Each is used in exactly one function of its own (%CO-SWITCH-1, %CO-START):
;;;; nothing may be live across them.  The heap is shared (r19/r20 are never
;;;; switched); the collector roots every parked thread through the table at
;;;; #x07240000 that %CO-PARK-TABLE writes before each switch, and scans the
;;;; running one's stack up to the Cheney block's stack_base (#x10000058), which
;;;; every switch sets to the target's stack top.
;;;;
;;;; PER-THREAD STATE: the handler state (in the save area, by the trap), a
;;;; stack, a handler-frame extension region, and the specials in *CO-SPECIALS*
;;;; -- the printer and stream variables and the runtime's per-computation state
;;;; (the list net/bare-threads.lisp binds for a Pi thread).  Specials are
;;;; SHALLOW-bound on ppc32: a LET writes the global cell, so these are saved
;;;; and restored at every switch.  Any other special a thread binds is seen by
;;;; the others while it is bound.
;;;;
;;;; MEMORY (boot/boot-ppcmac.lisp's map; DBAT0, cacheable):
;;;;   #x07240000            the parked table: count, then (save area, stack top)
;;;;   #x07240FF0            1 once CO-INIT has run
;;;;   #x07241000 + 4K*i     thread i's save area (+PPC-CO-SAVE-BYTES+, 2.2 KB)
;;;;   #x07260000 + 128K*(i-1)  thread i's handler-frame extension (4096 frames)
;;;;   [#x01800000 + 4M*(i-1), +4M)  thread i's stack (i = 1..4): between the
;;;;                         image (to ~#x01630000) and the native tier's code
;;;;                         (#x02800000).  No guard page: a canary at the bottom,
;;;;                         checked at every switch.
;;;; Thread 0's handler extension stays the boot's (#x07000000), its stack the
;;;; boot stack.

(defun %co-area (i) (+ #x07241000 (* i #x1000)))
(defun %co-max () 5)
(defun %co-stack-low (i) (+ #x01A00000 (* (- i 1) #x380000)))
(defun %co-stack-top-of (i) (+ #x01A00000 (* i #x380000)))
(defun %co-ext (i) (+ #x07260000 (* (- i 1) #x20000)))
(defun %co-active-p () (= (mem-ref #x07240FF0 :u32) 1))

;; kernel DEFVARs' initforms do not run: CO-INIT sets these
(defvar *co-current*)
(defvar *co-state*)          ; per thread: 0 free, 1 live, 2 dead
(defvar *co-fns*)
(defvar *co-tops*)           ; per thread: its stack top
(defvar *co-saved*)          ; per thread: its specials' values while parked
(defvar *co-specials*)

(defun %co-special-names ()
  "The specials a thread keeps of its own, by name: they live in COMMON-LISP,
   MODUS.RUNTIME (the condition system's, unexported) or MODUS-INT, so they are
   found at run time rather than read here."
  '("*STANDARD-OUTPUT*" "*STANDARD-INPUT*" "*ERROR-OUTPUT*" "*TRACE-OUTPUT*"
    "*QUERY-IO*" "*DEBUG-IO*" "*TERMINAL-IO*" "*PACKAGE*" "*READTABLE*"
    "*PRINT-BASE*" "*PRINT-RADIX*" "*PRINT-ESCAPE*" "*PRINT-PRETTY*" "*PRINT-READABLY*"
    "*PRINT-CIRCLE*" "*PRINT-CASE*" "*PRINT-LEVEL*" "*PRINT-LENGTH*" "*READ-BASE*"
    "*READ-EVAL*" "*MVM-LAST-MV*" "*NLX-STATE-SERIAL*" "*CURRENT-CONDITION*"
    "*CATCH-ACTIVE*" "*RESTART-STACK*" "*HANDLER-BIND-STACK*"
    "*HANDLER-BIND-EFFECTIVE-SKIP*" "*RESTART-FRAME-CONDITION-MAP*"
    "*SIGNAL-WALK-DEPTH*" "*RESTARTS-BEING-INVOKED*" "*RESTART-INVOKING-P*"
    "*RESTART-CASE-RESULT*" "*RC-INVOKED-RESTART*" "*FORMAT-ITER-ESCAPE*"
    "*WRITE-OBJECT-BUDGET*" "*%CIRC-NEXT*" "*%PPX-STACK*" "*%PP-CTX*"
    "*LOAD-ERROR-CONDITION*" "*%LOAD-DEPTH*" "*%NEXT-METHODS*" "*%CURRENT-GF-ARGS*"
    "*%CURRENT-GF*" "*%DMC-CALL-ARGS*" "*CATCH-TAG*" "*CATCH-VALUE*" "*CATCH-VALUES*"
    "*CATCH-TAGS*" "*JIT-NATIVE-RAN*" "*JIT-INHIBIT*"))

(defun %co-find-special (name)
  (let ((hit nil))
    (dolist (p '("COMMON-LISP" "MODUS.RUNTIME" "MODUS-INT") hit)
      (unless hit
        (let ((pkg (find-package p)))
          (when pkg
            (let ((s (find-symbol name pkg)))
              (when (and s (boundp s)) (setq hit s)))))))))

(defun %co-special-list ()
  (let ((out nil))
    (dolist (n (%co-special-names) out)
      (let ((s (%co-find-special n)))
        (when (and s (not (member s out))) (setq out (cons s out)))))))

(defun %co-fresh-state ()
  "A new thread's condition system and CATCH state: empty."
  (dolist (n '("*HANDLER-BIND-STACK*" "*RESTART-STACK*" "*CATCH-ACTIVE*"
               "*CURRENT-CONDITION*" "*CATCH-TAGS*" "*CATCH-TAG*" "*RESTARTS-BEING-INVOKED*"))
    (let ((s (%co-find-special n)))
      (when s (setf (symbol-value s) nil)))))

(defun co-init ()
  "Make the running computation thread 0.  Idempotent."
  (unless (%co-active-p)
    (setq *co-current* 0)
    (setq *co-state* (make-array (%co-max) :initial-element 0))
    (setq *co-fns* (make-array (%co-max) :initial-element nil))
    (setq *co-tops* (make-array (%co-max) :initial-element 0))
    (setq *co-specials* (%co-special-list))
    (setq *co-saved* (make-array (%co-max) :initial-element nil))
    (dotimes (i (%co-max))
      (setf (svref *co-saved* i) (make-array (length *co-specials*) :initial-element nil)))
    (setf (svref *co-state* 0) 1)
    (setf (svref *co-tops* 0) (mem-ref #x10000058 :u32))   ; the boot stack's base
    (setf (mem-ref #x07240000 :u32) 0)
    (setf (mem-ref #x07240FF0 :u32) 1))
  t)

(defun %co-save-specials (i)
  (let ((v (svref *co-saved* i)) (k 0))
    (dolist (s *co-specials*)
      (setf (svref v k) (symbol-value s))
      (setq k (+ k 1)))))

(defun %co-restore-specials (i)
  (let ((v (svref *co-saved* i)) (k 0))
    (dolist (s *co-specials*)
      (setf (symbol-value s) (svref v k))
      (setq k (+ k 1)))))

(defun %co-park-table (running)
  "The collector's parked table: every live thread but RUNNING."
  (let ((n 0) (a #x07240004))
    (dotimes (i (%co-max))
      (when (and (= (svref *co-state* i) 1) (/= i running))
        (setf (mem-ref a :u32) (%co-area i))
        (setf (mem-ref (+ a 4) :u32) (svref *co-tops* i))
        (setq a (+ a 8) n (+ n 1))))
    (setf (mem-ref #x07240000 :u32) n)))

(defun %co-canary-ok-p (i)
  (or (= i 0) (= (mem-ref (%co-stack-low i) :u32) #x0CA4A121)))

;; the two primitives, each alone in its function
(defun %co-switch-1 (from to) (%co-switch from to))

(defun %co-start (area top)
  ;; parent: 1 when it is resumed; child: on its own stack, and it must never
  ;; return from here or TAIL-call out of here -- its frame pointer is still the
  ;; parent's, so a return or a tail call would land on the parent's stack.
  ;; Hence the calls in non-tail positions and the LOOP after them.
  (if (eql (%co-fork area top) 0)
      (progn (%co-child-main) (%co-dead-loop) (loop))
      1))

(defun %co-switch-to (j)
  "Park the running thread, resume J (live, not running)."
  (let ((i *co-current*))
    (unless (%co-canary-ok-p i)
      (write-string-serial "co: STACK OVERFLOW in thread ") (print-dec i) (write-char-serial 10))
    (%co-save-specials i)
    (%co-park-table j)
    (setf (mem-ref #x10000058 :u32) (svref *co-tops* j))
    (setq *co-current* j)
    (%co-restore-specials j)
    (%co-switch-1 (%co-area i) (%co-area j))
    ;; resumed: whoever switched here restored ours and set *CO-CURRENT*
    nil))

(defun %co-next (i)
  (let ((j i))
    (dotimes (k (%co-max) i)
      (setq j (if (< (+ j 1) (%co-max)) (+ j 1) 0))
      (when (and (/= j i) (= (svref *co-state* j) 1)) (return j)))))

(defun co-yield ()
  "Let the next live thread run; back here when its turn comes round.  Free
   when threads are off or there is no other thread."
  (when (%co-active-p)
    (let ((j (%co-next *co-current*)))
      (when (/= j *co-current*) (%co-switch-to j))))
  nil)

(defun co-yield-if-due ()
  "CO-YIELD when this thread has run 2 ms since the last yield: the board's
   IO-DELAY calls it, so counted waits stay fast and the others still run."
  (when (%co-active-p)
    (let* ((now (logand (get-internal-real-time) #x3FFFFFFF))
           (last (mem-ref #x07240FF8 :u32)))
      (when (> (logand (- now last) #x3FFFFFFF) 2000)
        (co-yield)
        (setf (mem-ref #x07240FF8 :u32) (logand (get-internal-real-time) #x3FFFFFFF)))))
  nil)

(defun co-spawn (fn)
  "Run FN in a new thread, at once; this one resumes at its next turn.  The
   thread's number, or NIL when all are in use."
  (co-init)
  (let ((k nil))
    (dotimes (i (%co-max))
      (when (and (null k) (> i 0) (/= (svref *co-state* i) 1)) (setq k i)))
    (when k
      (let ((parent *co-current*))
        (setf (svref *co-fns* k) fn)
        (setf (svref *co-state* k) 1)
        (setf (svref *co-tops* k) (%co-stack-top-of k))
        (setf (mem-ref (%co-stack-low k) :u32) #x0CA4A121)
        (%co-save-specials parent)
        (%co-park-table k)
        (setf (mem-ref #x10000058 :u32) (%co-stack-top-of k))
        (setq *co-current* k)
        (%co-start (%co-area parent) (%co-stack-top-of k))
        k))))

(defun %co-child-main ()
  ;; the new thread: its own handler-frame extension, an empty condition
  ;; system, its function, and then it is gone
  (let ((k *co-current*))
    (setf (mem-ref #x100009D0 :u32) (%co-ext k))
    (setf (mem-ref #x100009D8 :u32) #x1000)
    (%co-fresh-state)
    (handler-case (funcall (svref *co-fns* k))
      (error (c) (write-string-serial "co: thread ") (print-dec k)
                 (write-string-serial " ended by an error") (write-char-serial 10)))
    (setf (svref *co-state* k) 2)
    (setf (svref *co-fns* k) nil)))

(defun %co-dead-loop ()
  ;; a finished thread hands the CPU on for good (nothing resumes it: dead)
  (loop (let ((j (%co-next *co-current*)))
          (if (/= j *co-current*)
              (%co-switch-to j)
              (return nil)))))

(defun co-threads ()
  "(number state) for each thread slot in use: state :live or :dead."
  (let ((out nil))
    (when (%co-active-p)
      (dotimes (i (%co-max))
        (let ((s (svref *co-state* i)))
          (when (> s 0) (setq out (cons (list i (if (= s 1) :live :dead) (if (= i *co-current*) :running :parked)) out))))))
    (reverse out)))

;;; The serial REPL's wait for a key yields to the other threads: that override
;;; of lib/serial-repl.lisp's %CONSOLE-READ-CHAR is spliced AFTER the REPL by
;;; mvm/build-cl-repl-common.lisp (here it lost to it, last DEFUN wins, and a
;;; REPL waiting for a key starved the SSH server: its second login was refused).
