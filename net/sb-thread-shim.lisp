;;;; sb-thread-shim.lisp — the SB-THREAD SURFACE, over modus's own primitives.
;;;;
;;;; This file is NOT compiled into the image.  It is carried as a source string
;;;; and evaluated at boot, for the same reason net/genera-compat.lisp is: it
;;;; defines symbols in a package that does not exist on the host reading the
;;;; build, and CHECK-PARSES reads every first-party build source with SBCL's
;;;; own reader.  `(defpackage "SB-THREAD" …)' evaluated host-side would be a
;;;; package-lock violation against SBCL's real one.
;;;;
;;;; WHAT THIS IS FOR.  The campaign is to run the ACTUAL glass compositor on
;;;; modus.  glass is READ-ONLY, so glass keeps saying `sb-thread:make-thread'
;;;; and modus has to mean something by it.  There is no `#-sb-thread' arm to
;;;; add and no port to write: modus moves.
;;;;
;;;; PORTABLE-SHAPED, NOT GLASS-SHAPED.  Every function here takes the arguments
;;;; SBCL's does, in SBCL's order, with SBCL's return values — not the subset
;;;; glass happens to pass.  Where that is not true it says PARTIAL at the
;;;; definition and the reason.  A stub that silently does the wrong thing for
;;;; an argument nobody tested is worse than an absent one, so an argument that
;;;; is not implemented SIGNALS rather than being ignored.
;;;;
;;;; ================= THE PARTIAL LIST, IN ONE PLACE =================
;;;; Read this before trusting any of it.  Each is repeated at its definition.
;;;;
;;;;   MAKE-THREAD        :EPHEMERAL is accepted and ignored (modus has no
;;;;                      concept of a thread the image will not wait for).
;;;;                      15 threads maximum, and the 16th SIGNALS.
;;;;   JOIN-THREAD        The value is the thread body's return value ONLY while
;;;;                      that thread's slot has not been reused; see the note
;;;;                      at the definition.  :DEFAULT and :TIMEOUT are real.
;;;;   TERMINATE-THREAD   NOT IMPLEMENTED — signals.  There is no way to unwind
;;;;                      another thread in modus and no plan for one.
;;;;   INTERRUPT-THREAD   NOT IMPLEMENTED — signals.  Same reason.
;;;;   WITH-MUTEX         :VALUE is not supported (signals if passed).  :WAIT-P
;;;;                      and :TIMEOUT are real.
;;;;   CONDITION-WAIT     Modus's condition variable REQUIRES THE MUTEX HELD
;;;;                      ACROSS SIGNAL AND BROADCAST — pthreads permits
;;;;                      signalling without it, this does not.  glass already
;;;;                      holds it (src/rfb.lisp:749).
;;;;   MUTEX/WAITQUEUE/SEMAPHORE
;;;;                      Their futex words come from an arena that is NEVER
;;;;                      RECLAIMED.  65536 for the life of the process; see
;;;;                      %SYNC-CELL in net/hosted-sync.lisp.
;;;;   *CURRENT-THREAD*   On the MAIN thread it is a thread object made once at
;;;;                      install time.  A thread created by MAKE-THREAD gets
;;;;                      its own.  A thread modus started some other way (an
;;;;                      actor scheduler thread) has none and reads the main
;;;;                      thread's — stated because it is a real hole.
;;;;   THREAD-YIELD       sched_yield(2).  Real.
;;;; ==================================================================

(defpackage "SB-THREAD"
  (:use)
  (:export "THREAD" "THREADP" "MAKE-THREAD" "JOIN-THREAD" "THREAD-NAME"
           "THREAD-ALIVE-P" "*CURRENT-THREAD*" "MAIN-THREAD-P" "ALL-THREADS"
           "THREAD-YIELD" "TERMINATE-THREAD" "INTERRUPT-THREAD"
           "MUTEX" "MUTEXP" "MAKE-MUTEX" "MUTEX-NAME" "MUTEX-OWNER"
           "WITH-MUTEX" "WITH-RECURSIVE-LOCK" "GRAB-MUTEX" "RELEASE-MUTEX"
           "HOLDING-MUTEX-P"
           "WAITQUEUE" "MAKE-WAITQUEUE" "WAITQUEUE-NAME"
           "CONDITION-WAIT" "CONDITION-NOTIFY" "CONDITION-BROADCAST"
           "SEMAPHORE" "MAKE-SEMAPHORE" "SEMAPHORE-NAME" "SEMAPHORE-COUNT"
           "SIGNAL-SEMAPHORE" "WAIT-ON-SEMAPHORE"
           "THREAD-ERROR" "JOIN-THREAD-ERROR" "INTERRUPT-THREAD-ERROR"))

;;; ============================================================
;;; BRINGING THE PROCESS UP TO MULTITHREADED, ONCE
;;; ============================================================
;;;
;;; Three things have to be true before a second thread may run REAL LISP, and
;;; none of them is true in a fresh ./modus because none of them is free:
;;;
;;;   1. THE ACTIVE-REGION CELL MUST BE PER CPU.  Otherwise every thread's
;;;      %GC-SET-REGION writes one shared word and a thread adopting its own
;;;      heap moves the main thread's out from under it.  This is also the
;;;      condition under which %MAKE-NATIVE-THREAD gives a thread a GC region at
;;;      all (see A THREAD THAT CAN CONS in net/hosted-sync.lisp), so without it
;;;      MAKE-THREAD hands back a thread that cannot cons.
;;;   2. THE MAIN THREAD MUST HAVE A REAL PER-CPU BLOCK, stamped CPU 0, BEFORE
;;;      (1) — a per-CPU read with no per-CPU block is a GS-relative load at
;;;      absolute address 16.
;;;   3. THE RUNTIME-TABLE LOCK MUST BE ON.  SYMBOL-VALUE, INTERN and FORMAT all
;;;      walk shared mutable tables, and everything the locked section allocates
;;;      has to land in region 0 or it is garbage the moment its own thread
;;;      collects.  %RT-THREADS-ON is what says so, and it REFUSES unless (1)
;;;      already holds.
;;;
;;; IT HAPPENS ON THE FIRST MAKE-THREAD AND IS NEVER UNDONE.  That is a
;;; deliberate choice over doing it at boot: a ./modus that never starts a
;;; thread pays nothing and keeps a GS base of 0, which is the state every
;;; single-threaded test in the tree measures.

(defvar *sb-threads-up* nil
  "T once the process has been switched to the multithreaded configuration.")

;;; GATE ON THE WORDS, NOT ON THE CALLS HAVING BEEN MADE.  The three steps
;;; below each have a documented refusal path — %RT-THREADS-ON returns 0 when
;;; the mode word is not set, and returns BEFORE it carves B-lite's arena and
;;; BEFORE it opens the gate — and the first version of this function DISCARDED
;;; every one of those return values and ended in an unconditional `t'.  A
;;; process in which bringup had silently half-happened was then
;;; indistinguishable from a healthy one, AND the flag was latched, so no later
;;; call could ever retry.  An unsafe configuration that cannot be told from a
;;; safe one is the thing that hides for four rounds, so this SIGNALS.
;;;
;;; ***READ THE WORDS THROUGH %HA-PERCPU-MODE AND %RT-THREADS-LIVE-P, NEVER AS
;;; AN INLINE (MEM-REF #x10000FF8 :U32) HERE.***  This file is EVALUATED from a
;;; baked source string, and an inline MEM-REF of a literal address, in a form
;;; that also calls a function defined by that string, reads the value the word
;;; held BEFORE the form began — measured, deterministically, and it is the
;;; single instrument artifact that produced this campaign's "the gate is 0"
;;; reports.  See the block at the top of test/hosted-bringup-bare.lisp.  The
;;; two accessors are ordinary compiled functions in the image and are honest.

(defun %sb-threads-up ()
  "Make the process safe for a second thread to run Lisp in.  Idempotent.
   Returns T, or NIL if the actor band could not be carved (in which case
   MAKE-THREAD will refuse rather than hand back something broken).

   SIGNALS if the band carved but the seam did not actually arm, rather than
   latching a partial success: with the per-CPU mode word clear every
   %GC-SET-REGION writes one shared word, and with the threads-live gate clear
   the runtime-table lock is INERT, so SYMBOL-VALUE, INTERN and the macro
   tables run unsynchronised on every worker.  Both are silent corruption."
  (if *sb-threads-up*
      t
      (if (zerop (%ha-carve))
          nil
          (progn
            (%ha-percpu-init-cpu (%ha-percpu-base) 0)
            (%ha-set-percpu-mode 1)
            (%rt-threads-on)
            (if (zerop (%ha-percpu-mode))
                (error "sb-thread: modus could not bring this process up to ~
                        multithreaded — the per-CPU active-region mode word ~
                        (#x10000FF8) is still 0 after %HA-SET-PERCPU-MODE.  ~
                        Every thread would share one active-region cell.")
                (if (zerop (%rt-threads-live-p))
                    (error "sb-thread: modus could not bring this process up ~
                            to multithreaded — the per-CPU mode word is set ~
                            but the threads-live gate (#x10000DB8) is still 0, ~
                            so %RT-THREADS-ON refused.  The runtime-table lock ~
                            would be inert and B-lite's arena uncarved.")
                    (progn
                      (setq *sb-threads-up* t)
                      t)))))))

;;; ============================================================
;;; THREADS
;;; ============================================================

(defclass sb-thread::thread ()
  ((name :initarg :name :initform nil :accessor sb-thread::thread-name)
   ;; The thread-table slot %MAKE-NATIVE-THREAD returned, or NIL for a thread
   ;; object that stands for a thread this layer did not start (the main one).
   (slot :initarg :slot :initform nil :accessor %thread-slot)
   ;; A CONS the body's return value is stored into.  A cons and not a slot
   ;; because the writer is the OTHER thread: see the note at JOIN-THREAD.
   (box :initarg :box :initform nil :accessor %thread-box)
   (joined :initform nil :accessor %thread-joined)))

(defun sb-thread::threadp (x) (typep x 'sb-thread::thread))

(defvar sb-thread:*current-thread*
  (make-instance 'sb-thread::thread :name "main thread" :slot nil)
  "PARTIAL.  Correct on the main thread and on any thread MAKE-THREAD started.
   A thread modus started by some other route — an actor scheduler thread —
   reads the main thread's object, because nothing rebinds this for it.")

(defvar *sb-main-thread* sb-thread:*current-thread*)
(defvar *sb-all-threads* nil
  "Every live thread this layer started, newest first.  Guarded by
   *SB-THREAD-REGISTRY-LOCK*: it is an ordinary Lisp list and two threads
   pushing at once would lose one.")

(defvar *sb-thread-registry-lock* nil)

(defun %sb-registry-lock ()
  (if (null *sb-thread-registry-lock*)
      (setq *sb-thread-registry-lock* (%sync-cell))
      0)
  *sb-thread-registry-lock*)

(defun sb-thread::all-threads ()
  (cons *sb-main-thread* *sb-all-threads*))

(defun sb-thread::main-thread-p (&optional (thread sb-thread:*current-thread*))
  (eq thread *sb-main-thread*))

(defun sb-thread::thread-yield ()
  "sched_yield(2) — syscall 24 on x86-64."
  (syscall3 24 0 0 0)
  nil)

(defun sb-thread::make-thread (function &key name arguments ephemeral)
  "Start an OS thread running FUNCTION and return a THREAD object.

   ARGUMENTS is applied to FUNCTION, exactly as SBCL does it.
   NAME is carried on the object and is otherwise decoration; modus does not
   name threads to the kernel.

   PARTIAL — :EPHEMERAL is accepted and IGNORED.  In SBCL it marks a thread the
   image need not wait for at save time; modus does not save images and has no
   corresponding concept, so there is nothing to do with it and nothing that
   could go wrong by ignoring it.  It is accepted rather than refused so that
   portable code passing it does not have to be edited.

   FIFTEEN THREADS.  The thread table has 16 slots and slot 0 is the main
   thread's, so 15 may be live at once; the 16th SIGNALS rather than returning
   NIL, because a caller that gets NIL from MAKE-THREAD will dereference it.
   A joined thread's slot is reused."
  ephemeral
  (if (not (%sb-threads-up))
      (error "sb-thread:make-thread: modus could not carve the actor band, so ~
              this image cannot start a thread.")
      (let* ((box (cons nil nil))
             (thread (make-instance 'sb-thread::thread :name name :box box))
             (body (lambda ()
                     ;; THE THREAD'S OWN *CURRENT-THREAD*.  A plain SETQ would
                     ;; write the process-wide global; this is a DYNAMIC binding
                     ;; around the body, which is what SBCL's is.
                     (let ((sb-thread:*current-thread* thread))
                       (setf (car box)
                             (if arguments
                                 (apply function arguments)
                                 (funcall function)))
                       (setf (cdr box) t))
                     0))
             (slot (%make-native-thread body)))
        (if (< slot 0)
            (error "sb-thread:make-thread: no thread could be started (code ~D). ~
                    -1 = all 15 thread slots are in use, -2 = the thread page ~
                    could not be mapped, -3 = no stack, -4 = clone failed, ~
                    -5 = the child never acknowledged its slot."
                   slot)
            (progn
              ;; SETF OF A SLOT, NOT OF AN ACCESSOR.  Measured on this image:
              ;; (setf (foreign-package:accessor obj) v) errors, while
              ;; (setf (slot-value obj 'name) v) works.  Reading through the
              ;; accessor is fine, so the readers stay.
              (setf (slot-value thread 'slot) slot)
              (%mutex-lock (%sb-registry-lock))
              (setq *sb-all-threads* (cons thread *sb-all-threads*))
              (%mutex-unlock (%sb-registry-lock))
              thread)))))

(defun sb-thread::thread-alive-p (thread)
  (let ((slot (%thread-slot thread)))
    (cond ((null slot) t)                       ; the main thread
          ((%thread-joined thread) nil)
          (t (= (%native-thread-alive-p slot) 1)))))

(defun %sb-forget-thread (thread)
  (%mutex-lock (%sb-registry-lock))
  (setq *sb-all-threads* (remove thread *sb-all-threads*))
  (%mutex-unlock (%sb-registry-lock)))

(defun sb-thread::join-thread (thread &key (default :%sb-no-default) timeout)
  "Wait for THREAD and return what its function returned.

   TIMEOUT is in SECONDS and may be a ratio (glass passes 1/60 elsewhere).  On
   expiry: DEFAULT if one was supplied, otherwise a JOIN-THREAD-ERROR — SBCL's
   contract exactly.

   THE RETURNED VALUE IS A POINTER INTO A REGION THAT IS ABOUT TO BE REUSABLE,
   AND THAT IS A REAL LIMITATION, NOT A THEORETICAL ONE.  A thread allocates in
   its OWN GC region.  Its return value is therefore an object in that region,
   and joining FREES THE SLOT — so the next MAKE-THREAD may take the same slot,
   re-initialise the same region, and the value silently becomes garbage.  It is
   valid until then and no longer.  If you need it to outlive the join, COPY IT
   (or have the thread return a fixnum, which is immediate and has no region).
   The value is read out BEFORE the slot is released, so a join whose result is
   used immediately is safe.

   Making this unconditionally safe means either collecting into region 0 under
   the runtime lock on the way out, or a handshake that copies; neither is done
   here and pretending otherwise would be the worst of the three."
  (let ((slot (%thread-slot thread)))
    (if (null slot)
        (error 'simple-error :format-control
               "sb-thread:join-thread: cannot join the main thread.")
        (let* ((deadline (if timeout
                             (+ (%monotonic-ns)
                                (truncate (* timeout 1000000000) 1))
                             nil))
               (ok nil))
          ;; POLL WITH A SLEEP, NOT A SPIN.  %JOIN-NATIVE-THREAD's budget is a
          ;; spin count, which is a core burned for the length of the wait and
          ;; is not a duration.  1 ms of sleep per turn makes a 2-second join
          ;; cost 2000 syscalls and no CPU.
          (loop
            (when (zerop (%native-thread-alive-p slot))
              (setq ok t)
              (return nil))
            (when (and deadline (>= (%monotonic-ns) deadline))
              (return nil))
            (%sleep-ms 1))
          (if ok
              (let ((box (%thread-box thread)))
                ;; The value FIRST, the slot afterwards: releasing the slot is
                ;; what makes the region reusable.
                (let ((value (car box)))
                  (setf (slot-value thread 'joined) t)
                  (%join-native-thread slot 1000)
                  (%sb-forget-thread thread)
                  value))
              (if (eq default :%sb-no-default)
                  (error 'simple-error :format-control
                         "sb-thread:join-thread: timed out.")
                  default
))))))

(defun sb-thread::terminate-thread (thread)
  "NOT IMPLEMENTED, deliberately.  Unwinding another thread means delivering a
   signal to it and running its unwind handlers on its own stack, and modus has
   neither a signal handler nor a per-thread unwind entry point.  This SIGNALS
   rather than returning quietly, because a caller that believes a thread was
   terminated and it was not is worse off than one that gets an error."
  thread
  (error "sb-thread:terminate-thread is not implemented on modus."))

(defun sb-thread::interrupt-thread (thread function)
  "NOT IMPLEMENTED, deliberately.  Same reason as TERMINATE-THREAD."
  thread function
  (error "sb-thread:interrupt-thread is not implemented on modus."))

;;; ============================================================
;;; MUTEXES
;;; ============================================================

(defclass sb-thread::mutex ()
  ((name :initarg :name :initform nil :accessor sb-thread::mutex-name)
   ;; The raw arena cell.  +0x00 is the futex word, +0x10 the owner, +0x18 the
   ;; recursion depth.  A number and not a Lisp object: the collector copies.
   (cell :initarg :cell :accessor %mutex-cell)))

(defun sb-thread::mutexp (x) (typep x 'sb-thread::mutex))

(defun sb-thread::make-mutex (&key name)
  (let ((cell (%sync-cell)))
    (if (zerop cell)
        (error "sb-thread:make-mutex: the synchronisation arena is exhausted ~
                (~D cells handed out, ~D refusals).  Cells are never reclaimed; ~
                see %SYNC-CELL in net/hosted-sync.lisp."
               (%sync-cells-handed-out) (%sync-cells-exhausted))
        (make-instance 'sb-thread::mutex :name name :cell cell))))

(defun sb-thread::mutex-owner (mutex)
  "PARTIAL.  SBCL returns the owning THREAD OBJECT; this returns the owning
   thread's CPU id, or NIL when free.  Modus's lock ownership is a CPU id (one
   GS-relative load) rather than a thread pointer, deliberately — see the
   runtime-table lock — and inventing a mapping back to a thread object would be
   a table this layer would then have to keep correct."
  (let ((o (%gc-read64 (+ (%mutex-cell mutex) #x10))))
    (if (zerop o) nil (- o 1))))

(defun sb-thread::holding-mutex-p (mutex)
  (let ((o (%gc-read64 (+ (%mutex-cell mutex) #x10))))
    (if (zerop o) nil (= o (+ (%thr-cpu) 1)))))

(defun sb-thread::grab-mutex (mutex &key (waitp t) timeout)
  "Acquire MUTEX.  T if acquired, NIL if WAITP was NIL and it was held, or if
   TIMEOUT (seconds) expired."
  (let ((cell (%mutex-cell mutex)))
    (cond
      ((not waitp)
       (if (= (%mutex-trylock cell) 1)
           (progn (%gc-write64 (+ cell #x10) (+ (%thr-cpu) 1)) t)
           nil))
      ((null timeout)
       (%mutex-lock cell)
       (%gc-write64 (+ cell #x10) (+ (%thr-cpu) 1))
       t)
      (t
       (let ((deadline (+ (%monotonic-ns) (truncate (* timeout 1000000000) 1)))
             (got nil))
         (loop
           (when (= (%mutex-trylock cell) 1) (setq got t) (return nil))
           (when (>= (%monotonic-ns) deadline) (return nil))
           (%sleep-ms 1))
         (if got
             (progn (%gc-write64 (+ cell #x10) (+ (%thr-cpu) 1)) t)
             nil))))))

(defun sb-thread::release-mutex (mutex &key if-not-owner)
  "Release MUTEX.  IF-NOT-OWNER is :PUNT (do nothing), :WARN or :ERROR, as in
   SBCL; the default is :PUNT."
  (let ((cell (%mutex-cell mutex)))
    (if (sb-thread::holding-mutex-p mutex)
        (progn (%gc-write64 (+ cell #x10) 0)
               (%mutex-unlock cell)
               nil)
        (cond ((eq if-not-owner :error)
               (error "sb-thread:release-mutex: not the owner."))
              ((eq if-not-owner :warn)
               (format *error-output*
                       "~&sb-thread:release-mutex: not the owner.~%")
               nil)
              (t nil)))))

(defmacro sb-thread:with-mutex ((mutex &rest options) &body body)
  "SBCL's WITH-MUTEX.  Options :WAIT-P and :TIMEOUT are honoured; when either
   says the lock was not acquired, the body is NOT run and the form is NIL.

   PARTIAL — :VALUE IS NOT SUPPORTED and SIGNALS AT MACROEXPANSION.  In SBCL it
   names the value to store as the owner in place of the current thread, which
   only means something to a lock whose owner is a thread pointer; modus's owner
   is a CPU id.  Signalling at expansion rather than ignoring it is the point:
   a silently dropped :VALUE would make a lock look held by the wrong thing."
  (let ((waitp t) (timeout nil) (rest options))
    (loop
      (when (null rest) (return nil))
      (cond ((eq (car rest) :wait-p) (setq waitp (cadr rest)))
            ((eq (car rest) :timeout) (setq timeout (cadr rest)))
            ((eq (car rest) :value)
             (error "sb-thread:with-mutex :value is not supported on modus ~
                     (its lock owner is a CPU id, not a thread object)."))
            (t (error "sb-thread:with-mutex: unknown option ~S" (car rest))))
      (setq rest (cddr rest)))
    (let ((m (gensym "MUTEX")) (got (gensym "GOT")))
      `(let* ((,m ,mutex)
              (,got (sb-thread::grab-mutex ,m :waitp ,waitp :timeout ,timeout)))
         (if ,got
             (unwind-protect (progn ,@body)
               (sb-thread::release-mutex ,m))
             nil)))))

;;; A RECURSIVE LOCK IS A DIFFERENT OBJECT'S BEHAVIOUR, NOT A DIFFERENT LOCK.
;;; SBCL's WITH-RECURSIVE-LOCK takes an ordinary MUTEX and makes re-entry by the
;;; same thread legal.  So the depth lives in the cell and only this macro reads
;;; it; WITH-MUTEX stays non-recursive, which is what makes a genuine re-entrant
;;; deadlock still show up as one.
(defmacro sb-thread:with-recursive-lock ((mutex &rest options) &body body)
  (let ((m (gensym "MUTEX")) (cell (gensym "CELL")) (mine (gensym "MINE")))
    (declare (ignore options))
    `(let* ((,m ,mutex)
            (,cell (%mutex-cell ,m))
            (,mine (sb-thread::holding-mutex-p ,m)))
       (if ,mine
           (progn
             (%gc-write64 (+ ,cell #x18) (+ (%gc-read64 (+ ,cell #x18)) 1))
             (unwind-protect (progn ,@body)
               (%gc-write64 (+ ,cell #x18)
                            (- (%gc-read64 (+ ,cell #x18)) 1))))
           (progn
             (sb-thread::grab-mutex ,m)
             (unwind-protect (progn ,@body)
               (sb-thread::release-mutex ,m)))))))

;;; ============================================================
;;; CONDITION VARIABLES
;;; ============================================================

(defclass sb-thread::waitqueue ()
  ((name :initarg :name :initform nil :accessor sb-thread::waitqueue-name)
   (cell :initarg :cell :accessor %waitqueue-cell)))

(defun sb-thread::make-waitqueue (&key name)
  (let ((cell (%sync-cell)))
    (if (zerop cell)
        (error "sb-thread:make-waitqueue: the synchronisation arena is exhausted.")
        (make-instance 'sb-thread::waitqueue :name name :cell cell))))

(defun sb-thread::condition-wait (queue mutex &key timeout)
  "Atomically release MUTEX and wait on QUEUE; re-acquire MUTEX before
   returning.  T if woken, NIL if TIMEOUT (seconds, may be a ratio) expired.

   SPURIOUS WAKEUPS ARE PERMITTED, exactly as in SBCL: T does not mean the
   predicate is true.  Call this in a loop.

   PARTIAL, AND IT IS A REQUIREMENT ON THE CALLER RATHER THAN A MISSING FEATURE:
   MODUS'S CONDITION VARIABLE REQUIRES THE ASSOCIATED MUTEX TO BE HELD ACROSS
   CONDITION-NOTIFY AND CONDITION-BROADCAST.  pthreads permits signalling
   without it; this does not, because the sequence counter's increment is a
   plain read-modify-write and this ISA has an unconditional exchange but no
   atomic add.  Signalling without the mutex can lose a wakeup."
  (let* ((cv (+ (%waitqueue-cell queue) #x08))
         (mcell (%mutex-cell mutex))
         (raw (if timeout (truncate (* timeout 1000) 1) 0))
         ;; A timeout that rounds to zero milliseconds must not become "no
         ;; timeout" — that is an infinite park where the caller asked for a
         ;; brief one.  Floor it at 1 ms.
         (ms (if timeout (if (< raw 1) 1 raw) 0)))
    ;; The owner word is this layer's, not the futex's: %COND-WAIT-MS drops and
    ;; re-takes the raw mutex, so the ownership record has to follow it.
    (%gc-write64 (+ mcell #x10) 0)
    (let ((r (%cond-wait-ms cv mcell ms)))
      (%gc-write64 (+ mcell #x10) (+ (%thr-cpu) 1))
      (if (zerop r) t nil))))

(defun sb-thread::condition-notify (queue &optional (n 1))
  "Wake at most N waiters.  THE ASSOCIATED MUTEX MUST BE HELD; see
   CONDITION-WAIT."
  (let ((cv (+ (%waitqueue-cell queue) #x08)))
    (%cond-bump cv)
    (%futex-wake cv n)
    nil))

(defun sb-thread::condition-broadcast (queue)
  "Wake every waiter.  THE ASSOCIATED MUTEX MUST BE HELD; see CONDITION-WAIT."
  (%cond-broadcast (+ (%waitqueue-cell queue) #x08))
  nil)

;;; ============================================================
;;; SEMAPHORES
;;; ============================================================
;;;
;;; A counting semaphore over the mutex and the condition variable in the same
;;; cell, which is why it is a dozen lines and not a new primitive: +0x00 is the
;;; mutex, +0x08 the condvar, +0x20 the count.

(defclass sb-thread::semaphore ()
  ((name :initarg :name :initform nil :accessor sb-thread::semaphore-name)
   (cell :initarg :cell :accessor %semaphore-cell)))

(defun sb-thread::make-semaphore (&key name (count 0))
  (let ((cell (%sync-cell)))
    (if (zerop cell)
        (error "sb-thread:make-semaphore: the synchronisation arena is exhausted.")
        (progn (%gc-write64 (+ cell #x20) count)
               (make-instance 'sb-thread::semaphore :name name :cell cell)))))

(defun sb-thread::semaphore-count (semaphore)
  (%gc-read64 (+ (%semaphore-cell semaphore) #x20)))

(defun sb-thread::signal-semaphore (semaphore &optional (n 1))
  (let ((cell (%semaphore-cell semaphore)))
    (%mutex-lock cell)
    (%gc-write64 (+ cell #x20) (+ (%gc-read64 (+ cell #x20)) n))
    ;; The mutex is held across the bump AND the wake, which is what modus's
    ;; condition variable requires.
    (%cond-bump (+ cell #x08))
    (%futex-wake (+ cell #x08) n)
    (%mutex-unlock cell)
    nil))

(defun sb-thread::wait-on-semaphore (semaphore &key timeout (n 1))
  "Decrement by N when the count allows it.  Returns the count AFTER the
   decrement (SBCL's contract), or NIL on timeout."
  (let* ((cell (%semaphore-cell semaphore))
         (deadline (if timeout
                       (+ (%monotonic-ns) (truncate (* timeout 1000000000) 1))
                       nil))
         (result nil))
    (%mutex-lock cell)
    (loop
      (when (>= (%gc-read64 (+ cell #x20)) n)
        (%gc-write64 (+ cell #x20) (- (%gc-read64 (+ cell #x20)) n))
        (setq result (%gc-read64 (+ cell #x20)))
        (return nil))
      (when (and deadline (>= (%monotonic-ns) deadline)) (return nil))
      (%cond-wait-ms (+ cell #x08) cell (if deadline 5 0)))
    (%mutex-unlock cell)
    result))

;;; ============================================================
;;; CONDITIONS
;;; ============================================================

(define-condition sb-thread::thread-error (error) ())
(define-condition sb-thread::join-thread-error (sb-thread::thread-error) ())
(define-condition sb-thread::interrupt-thread-error (sb-thread::thread-error) ())

;;; ============================================================
;;; THE FEATURE
;;; ============================================================
;;;
;;; PUSHED ONLY IF EVERYTHING ABOVE INSTALLED.  `#+sb-thread' in portable code —
;;; glass/fb's framebuffer and clipboard locks are exactly this — means "the
;;; sb-thread surface is here", and a half-installed shim advertising it would
;;; send that code down the arm it cannot run.
(pushnew :sb-thread *features*)
