;;;; net/hosted-coroutines.lisp — COROUTINES on hosted x86-64: several stacks,
;;;; one OS thread, one heap.  docs/coroutines.md has the why and the plan.
;;;;
;;;; THE SHAPE.  A coroutine is the hosted actor runtime's notion of a
;;;; computation -- its own stack, its own per-thread WINDOW (handler frames,
;;;; dynamic bindings, multiple values, the stop-the-world words) -- with two
;;;; differences that are the whole point:
;;;;   * it has NO REGION of its own.  It allocates in region 0, as the main
;;;;     thread does, so what it stores into a global is an ordinary region-0
;;;;     store and the shared-store guard has nothing to refuse (its window's
;;;;     region words are 0: "no region of its own = nothing to guard");
;;;;   * it runs only on the MAIN OS thread, and control moves between
;;;;     coroutines by an explicit COROUTINE-TRANSFER, never by a scheduler.
;;;;     Exactly one computation is running at any instant, which is what a
;;;;     Lisp Machine's stack groups are and what cortez needs them to be.
;;;;
;;;; WHAT THE COLLECTOR SEES.  Threads are armed (stop-the-world), so every
;;;; region-0 collection takes the world and then scans:
;;;;   * its OWN stack, from RSP to the main thread record's stack top
;;;;     (+0x78, EMIT-X64-STW-OWN-WINDOW-TOP) -- which a transfer sets to the
;;;;     top of the stack being entered, together with region 0's stack_base
;;;;     (the window other threads use for main, and the unarmed path's);
;;;;   * its OWN window, through FS -- the running coroutine's;
;;;;   * every PARKED computation, through the parked-actor scan table
;;;;     (+STW-ACTOR-TABLE-ADDR+, EMIT-STW-SCAN-PARKED-ACTORS): stack from
;;;;     the SP its SAVE-CONTEXT recorded, window, and region 0 (no region).
;;;; Nothing in a transfer allocates, and objects move only at allocations, so
;;;; no collection can see a half-made switch.
;;;;
;;;; IT CANNOT RUN BESIDE THE ACTOR RUNTIME: both own the one parked-actor scan
;;;; table.  COROUTINES-START refuses when the actor runtime holds it.
;;;;
;;;; CONTROL PAGE (raw, mmap'd, outside every heap):
;;;;   +0x000 state (1 = started)    +0x008 current id      +0x010 slots made
;;;;   +0x018 main's stack top       +0x020 windows base    +0x028 dynb base
;;;;   +0x050 entry scratch word
;;;;   +0x100 + id*64  save area (+0x00 RSP +0x18 RBX +0x28 RIP +0x38 RBP)
;;;;   +0x1400 + id*48 parked scan entry (the actor runtime's format)
;;;;   +0x2400 + id*8  stack top     +0x2600 + id*8 stack mapping base
;;;;   +0x2800 + id*8  state (0 free, 1 made, 2 ran, 3 done)
;;;;   +0x2A00 + id*8  last resumer
;;;; Id 0 is the main computation: its stack is the process stack, its window
;;;; the main thread's own (segment base 0).

(defvar *co-ctl* 0)
(defvar *co-fns* nil "Coroutine id -> the function it runs (of one argument).")
(defvar *co-value* nil "The value in flight across a transfer.")

(defun %co-max () 64)
(defvar *co-stack-bytes* #x2000000) ; 32 MB, committed only as touched

(defun %co-get (off) (%gc-read64 (+ *co-ctl* off)))
(defun %co-set (off v) (%gc-write64 (+ *co-ctl* off) v))
(defun %co-save-area (id) (+ *co-ctl* (+ #x100 (* id 64))))
(defun %co-scan-entry (id) (+ *co-ctl* (+ #x1400 (* id 48))))
(defun %co-stack-top (id) (%co-get (+ #x2400 (* id 8))))
(defun %co-state (id) (%co-get (+ #x2800 (* id 8))))
(defun %co-set-state (id v) (%co-set (+ #x2800 (* id 8)) v))
(defun %co-window (id) (+ (%co-get #x20) (* id #x8000)))
(defun %co-dynb (id) (+ (%co-get #x28) (* id #x4000)))
(defun %co-delta (id) (if (zerop id) 0 (- (%co-window id) #x10000000)))

(defun coroutines-started-p () (and (> *co-ctl* 0) (= (%co-get 0) 1)))

(defun coroutines-start ()
  "Bring coroutines up on this (the main) thread: threads armed, so every
   region-0 collection scans parked computations; a control page; windows and
   binding stacks for every slot.  Once per process.  Returns T or signals."
  (when (coroutines-started-p) (return-from coroutines-start t))
  (unless (%mvm-on-main-thread-p)
    (error "coroutines-start: call it on the main thread."))
  (unless (zerop (%gc-read64 #x100050D0))
    (error "coroutines-start: the actor runtime holds the parked-computation table."))
  (when (zerop (%ha-carve)) (error "coroutines-start: the band could not be carved."))
  (%ha-percpu-init-cpu (%ha-percpu-base) 0)
  (%ha-set-percpu-mode 1)
  (%rt-threads-on)
  (when (zerop (%rt-threads-live-p)) (error "coroutines-start: threads could not be turned on."))
  (let ((m (%mmap-shared-page #x4000)))
    (when (< m 4096) (error "coroutines-start: no control page."))
    (%ha-zero m (+ m #x4000))
    (setq *co-ctl* m))
  (let ((w (%mmap-shared-page (* (%co-max) #x8000)))
        (d (%mmap-shared-page (* (%co-max) #x4000))))
    (when (or (< w 4096) (< d 4096)) (error "coroutines-start: could not map windows."))
    (%co-set #x20 w) (%co-set #x28 d))
  (setq *co-fns* (make-array (%co-max) :initial-element nil))
  ;; Id 0, the main computation: running, on the process stack.
  (let ((top (%gc-read64 (+ (%thr-rec 0) #x78))))
    (%co-set #x18 top)
    (%co-set #x2400 top))
  (%co-set-state 0 2)
  (%co-set #x08 0)
  (%co-set #x10 1)
  ;; The parked scan table, published to the collector last.
  (%gc-write64 #x100050D0 (+ *co-ctl* #x1400))
  (%co-set 0 1)
  t)

(defun %co-prepare-window (id)
  "Coroutine ID's window, clean, as %AR-PREPARE-WINDOW makes an actor's --
   empty binding stack, no handler frames, the self slot its own segment
   base -- but with NO REGION (the guard's words 0) and its own stack as the
   context stack."
  (let* ((b (%co-window id))
         (delta (- b #x10000000))
         (ext (%gc-read64 (+ b #x5600))))
    (%ha-zero b (+ b #x8000))
    (%gc-write64 (+ b #x5600) ext)
    (setf (mem-ref (+ b #xC60) :u64) (%co-dynb id))
    (%gc-write64 (+ b #x50C0) (%co-stack-top id))
    (%gc-write64 (+ b #x50C8) (- (%co-stack-top id) *co-stack-bytes*))
    (%gc-write64 (+ b #xC30) delta)
    delta))

(defun %co-entry-addr ()
  (- (%gc-word-of (fn-addr %co-entry) (+ *co-ctl* #x50)) 3))

(defun coroutine-create (fn)
  "A new coroutine that will run FN, a function of one argument: the value of
   the first transfer to it.  Returns its id.  It does not run until
   COROUTINE-TRANSFER enters it."
  (unless (coroutines-started-p) (coroutines-start))
  (let ((id 0) (i 1))
    (loop
      (when (>= i (%co-max)) (return 0))
      (let ((st (%co-state i)))
        (when (or (= st 0) (= st 3)) (setq id i) (return 0)))
      (setq i (+ i 1)))
    (when (zerop id) (error "coroutine-create: all ~D coroutines are in use." (- (%co-max) 1)))
    ;; A stack, once per slot: [guard page][stack], growing down from the top.
    (when (zerop (%co-get (+ #x2600 (* id 8))))
      (let ((s (%mmap-shared-page (+ *co-stack-bytes* 4096))))
        (when (< s 4096) (error "coroutine-create: no stack."))
        (syscall3 10 s 4096 0)
        (%co-set (+ #x2600 (* id 8)) s)
        (%co-set (+ #x2400 (* id 8)) (+ s 4096 *co-stack-bytes*))))
    (%co-prepare-window id)
    (setf (svref *co-fns* id) fn)
    ;; The first entry: RSP its stack top, RBX and RBP zero, RIP %CO-ENTRY.
    (let ((sa (%co-save-area id)))
      (%ha-zero sa (+ sa 64))
      (%gc-write64 sa (%co-stack-top id))
      (%gc-write64 (+ sa #x28) (%co-entry-addr)))
    ;; Parked from birth: an empty stack, its window.
    (%co-mark-parked id (%co-stack-top id))
    (%co-set-state id 1)
    id))

(defun %co-mark-parked (id sp)
  (let ((e (%co-scan-entry id)))
    (%gc-write64 (+ e 8) sp)
    (%gc-write64 (+ e 16) (%co-stack-top id))
    (%gc-write64 (+ e 24) (%co-delta id))
    (%gc-write64 (+ e 32) 0)
    (%gc-write64 e 1)))

(defun %co-mark-running (id) (%gc-write64 (%co-scan-entry id) 0))

(defun %co-enter (id)
  "Make ID the running computation and jump into it.  Never returns here."
  (let ((top (%co-stack-top id)) (k (%gc-meta-scale)))
    (%gc-write64 (+ (%thr-rec 0) #x78) top)
    (%gc-meta-write (+ (%gc-region-0) #x18) top k)
    (syscall3 158 #x1002 (%co-delta id) 0)
    (%gc-write64 (+ (%thr-rec 0) #x38) (%co-delta id))
    (%co-set #x08 id)
    (%co-mark-running id)
    (restore-context (%co-save-area id))))

(defun coroutine-current () (if (coroutines-started-p) (%co-get #x08) 0))

(defun coroutine-done-p (id) (= (%co-state id) 3))

(defun coroutine-transfer (id value)
  "Suspend the running computation and run coroutine ID, handing it VALUE: its
   first argument if it has not run yet, else what its own last
   COROUTINE-TRANSFER returns.  Returns the value the next transfer back to
   this computation hands over."
  (unless (coroutines-started-p) (error "coroutine-transfer: coroutines are not started."))
  (unless (%mvm-on-main-thread-p) (error "coroutine-transfer: only on the main thread."))
  (let ((from (%co-get #x08)))
    (cond ((= id from) value)
          ((or (< id 0) (>= id (%co-max)) (= (%co-state id) 0) (= (%co-state id) 3))
           (error "coroutine-transfer: there is no live coroutine ~D." id))
          (t
           (setq *co-value* value)
           (%co-set (+ #x2A00 (* id 8)) from)
           (if (zerop (save-context (%co-save-area from)))
               (progn
                 (%co-mark-parked from (%gc-read64 (%co-save-area from)))
                 (%co-enter id))
               (let ((v *co-value*)) (setq *co-value* nil) v))))))

(defun %co-entry ()
  "Where every coroutine starts: its function on the first value, inside its
   own computation state.  When the function returns, the coroutine is done
   and its value goes to whoever last resumed it."
  (let ((id (%co-get #x08)))
    (%co-set-state id 2)
    (let ((first *co-value*) (result nil))
      (setq *co-value* nil)
      (%with-computation-state ((* (+ id 80) 1099511627776))
        (handler-case (setq result (funcall (svref *co-fns* id) first))
          (serious-condition (e)
            (format t "~&modus: coroutine ~D ended with an unhandled error: ~A~%" id e)
            (setq result nil))))
      (setf (svref *co-fns* id) nil)
      (%co-set-state id 3)
      (let ((to (%co-get (+ #x2A00 (* id 8)))))
        (setq *co-value* result)
        ;; Done: nothing will scan this stack again, and nothing returns here.
        (%co-mark-running id)
        (%co-enter (if (or (= to id) (= (%co-state to) 3)) 0 to))))))
