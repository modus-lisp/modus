;;;; net/hosted-actor-runtime.lisp — the hosted actor RUNTIME (x86-64 Linux).
;;;;
;;;; net/actors.lisp is the scheduler core and net/hosted-sync.lisp gives it a
;;;; scheduler that blocks (%SCHED-RUN).  Everything that used them was a
;;;; selftest that wired its own band, regions and threads by hand.  This file
;;;; is the runtime a program uses: docs/hosted-actor-runtime.md has the plan
;;;; and the gap list this is built against.
;;;;
;;;; THE SHAPE.  M:N.  ACTORS-START brings the process up to threads (as the
;;;; sb-thread shim does), carves a GC region per actor slot out of region 0,
;;;; maps a stack and a per-thread-style WINDOW per slot, and starts N native
;;;; scheduler threads (the hardened %MAKE-NATIVE-THREAD kind), each running
;;;; %AR-SCHED-RUN.  The main thread is actor 1.
;;;;
;;;; AN ACTOR HAS ITS OWN WINDOW.  Everything the per-thread window holds —
;;;; handler frames and depth, the armed frame, the dynamic-binding stack, the
;;;; multiple-value buffer, the stop-the-world state words, the shared-store
;;;; guard's region words — belongs to the COMPUTATION, and an actor is a
;;;; computation that can stop in the middle (RECEIVE) while another runs on
;;;; the same thread.  So the scheduler points FS at the actor's window block
;;;; when it dispatches the actor and back at the thread's own when the actor
;;;; hands the thread back, and updates the thread record's segment word
;;;; (+0x38) with it: that word is how stop-the-world finds a thread's window.
;;;;
;;;; SWITCHES ONLY THROUGH THE SCHEDULER.  An actor that blocks, yields or ends
;;;; hands the thread back to %AR-SCHED-RUN (never straight to another actor),
;;;; parking its region at the SP its SAVE-CONTEXT recorded; the scheduler
;;;; enters the next actor's window and region.  One place does each half.
;;;;
;;;; CONTROL BLOCK (raw, mmap'd, outside every heap):
;;;;   +0x000 state (1 = started)   +0x008 slots      +0x010 region size
;;;;   +0x018 region pool (from)    +0x020 pool (to)  +0x028 scheduler threads
;;;;   +0x030 windows base          +0x038 dynb base  +0x040 stacks base
;;;;   +0x048 main wake word        +0x050 entry scratch  +0x058 staging base
;;;;   +0x080 scheduler thread slots (16 x 8)
;;;;   +0x200 + id*64  actor ID's region control block
;;;   +0x1400 + id*48 actor ID's parked-actor scan entry (%AR-MARK-PARKED)
;;;   +0x2400 + id*8  actor ID's receive deadline   +0x2800 + id*8 timed out

(defvar *ar-ctl* 0)
(defvar *ar-fns* nil
  "Actor id -> the function it runs.  Made by ACTORS-START on the main thread,
   so it is a region-0 object; an actor storing its OWN closure here would be a
   shared-store violation and is refused (spawn from an actor with a symbol).")

;;; ACTOR STACKS are adjacent slots of one mapping: actor ID's stack is
;;; [base + ID*size, base + (ID+1)*size), growing down.  256 KB was too small for
;;; real work (operandi's tool turn: interpreted frames are large), and an
;;; overflow ran straight into the slot BELOW -- the actor spawned just before,
;;; whose outermost frames it overwrote with NIL-initialised slots while that
;;; actor sat blocked in read(2).  It returned through them to 0xDEAD0001.  So:
;;; 2 MB each (pages are only committed when touched), and the lowest page of
;;; every slot is a PROT_NONE guard, so an overflow faults in the actor that
;;; overflowed instead of corrupting its neighbour.
(defun %ar-stack-bytes () #x200000)         ; 2 MB per actor, guard page at the bottom
(defun %ar-max-slots () 60)

(defun %ar-get (off) (%gc-read64 (+ *ar-ctl* off)))
(defun %ar-set (off v) (%gc-write64 (+ *ar-ctl* off) v))
(defun %ar-started-p () (and (> *ar-ctl* 0) (= (%ar-get 0) 1)))

(defun %ar-rcb (id) (+ *ar-ctl* (+ #x200 (* id 64))))
(defun %ar-window (id) (+ (%ar-get #x30) (* id #x8000)))
(defun %ar-dynb (id) (+ (%ar-get #x38) (* id #x4000)))
(defun %ar-stack-top (id) (+ (%ar-get #x40) (* (+ id 1) (%ar-stack-bytes))))
(defun %ar-region-from (id) (+ (%ar-get #x18) (* (- id 2) (%ar-get #x10))))
(defun %ar-region-to (id) (+ (%ar-get #x20) (* (- id 2) (%ar-get #x10))))

;;; ------------------------------------------------------------
;;; THE CARVE: one GC region per slot, out of region 0
;;; ------------------------------------------------------------
;;;
;;; Both semispaces lose the same top slice (the region's size is shared), so
;;; actor I's from/to are at the same offset in each.  The lock arena, carved
;;; earlier from one semispace's top, lies ABOVE the current size and is not
;;; touched.  The live frontier must be below the carve point: collect once to
;;; compact, as %HA-CARVE-ROOM does, and refuse if it still is not.

(defun %ar-carve (nslots size)
  (let* ((k (%gc-meta-scale))
         (r0 (%gc-region-0))
         (need (+ (* nslots size) #x200000)))
    (let ((from (%gc-meta-read r0 k))
          (sz (%gc-meta-read (+ r0 #x10) k)))
      (when (and (= r0 (%gc-region)) (> (get-alloc-ptr) (+ from (- sz need))))
        (%gc-collect-here))
      (let* ((from2 (%gc-meta-read r0 k))
             (to2 (%gc-meta-read (+ r0 #x08) k))
             (sz2 (%gc-meta-read (+ r0 #x10) k))
             (newsize (logand (- sz2 need) (- 0 #x100000))))
        (if (or (< newsize #x4000000)
                (and (= r0 (%gc-region)) (> (get-alloc-ptr) (+ from2 newsize))))
            0
            (progn
              (%gc-region-shrink r0 newsize k)
              (if (> (%gc-meta-read (+ r0 #x38) k) (+ from2 newsize))
                  (%gc-meta-write (+ r0 #x38) (+ from2 newsize) k)
                  0)
              (%ar-set #x18 (%ha-align-up-to-page-base (+ from2 (+ newsize #x100000))))
              (%ar-set #x20 (%ha-align-up-to-page-base (+ to2 (+ newsize #x100000))))
              (%ar-set #x10 size)
              1))))))

;;; ------------------------------------------------------------
;;; WINDOWS
;;; ------------------------------------------------------------

(defun %ar-prepare-window (id)
  "Actor ID's window, clean, as %TLS-PREPARE-BLOCK makes a thread's: empty
   dynamic-binding stack, no handler frames, stop-the-world state running, the
   self slot its own segment base, the guard's words its region pair, and the
   context stack top the collector bounds its stack scan with."
  (let* ((b (%ar-window id))
         (delta (- b #x10000000)))
    (%ha-zero b (+ b #x8000))
    (setf (mem-ref (+ b #xC60) :u64) (%ar-dynb id))
    (%gc-write64 (+ b #x5040) (%ar-region-from id))
    (%gc-write64 (+ b #x5048) (%ar-region-to id))
    (%gc-write64 (+ b #x5050) (%ar-get #x10))
    (%gc-write64 (+ b #x50C0) (%ar-stack-top id))
    (%gc-write64 (+ b #x50C8) (- (%ar-stack-top id) (%ar-stack-bytes)))
    (%gc-write64 (+ b #xC30) delta)
    delta))

(defun %ar-scan-entry (id) (+ *ar-ctl* (+ #x1400 (* id 48))))

(defun %ar-mark-parked (id sp)
  "Actor ID is off-CPU with its stack down to SP: the stop-the-world scan
   (translate-x64, EMIT-STW-SCAN-PARKED-ACTORS) reaches its stack, window and
   region through this entry.  Set BEFORE the thread leaves the actor, cleared
   by the actor itself once it runs on its own stack again, so there is no
   instant at which neither the entry nor a thread's scan covers it (the
   overlap only scans the same roots twice, which is harmless)."
  (let ((e (%ar-scan-entry id)))
    (%gc-write64 (+ e 8) sp)
    (%gc-write64 (+ e 16) (%ar-stack-top id))
    (%gc-write64 (+ e 24) (- (%ar-window id) #x10000000))
    (%gc-write64 (+ e 32) (%ar-rcb id))
    (%gc-write64 e 1)))

(defun %ar-mark-running (id) (%gc-write64 (%ar-scan-entry id) 0))

(defun %ar-set-fs (delta)
  "Point this thread's FS at the window whose segment base is DELTA, and tell
   stop-the-world (the thread record's +0x38) where this thread's window now is."
  (syscall3 158 #x1002 delta 0)
  (%gc-write64 (+ (%thr-rec (%thr-cpu)) #x38) delta)
  0)

(defun %ar-thread-delta ()
  (- (%thr-tls-block (%thr-cpu)) #x10000000))

;;; ------------------------------------------------------------
;;; THE SCHEDULER
;;; ------------------------------------------------------------

(defun %ar-thread-rcb ()
  (%gc-read64 (+ (%thr-rec (%thr-cpu)) #x40)))

;;; TIMEOUTS.  An actor blocking with a deadline records it at +0x2400+8*id
;;; (monotonic ns, 0 = none).  Every scheduler pass expires deadlines under
;;; the lock -- a BLOCKED actor (status 4) past its deadline becomes READY with
;;; its timed-out flag (+0x2800+8*id) set -- and an idle scheduler sleeps no
;;; longer than the earliest one.  Only status-4 actors are touched, so an
;;; expiry can never re-queue an actor that a message already woke.

(defun %ar-deadline-addr (id) (+ *ar-ctl* (+ #x2400 (* 8 id))))
(defun %ar-timedout-addr (id) (+ *ar-ctl* (+ #x2800 (* 8 id))))

(defun %ar-expire ()
  "Under the scheduler lock: wake every blocked actor past its deadline.
   Returns the earliest deadline still pending, or 0."
  (let ((now (%monotonic-ns)) (soonest 0) (n (+ (%ar-get #x08) 2)) (i 2))
    (loop
      (when (>= i n) (return 0))
      (let ((d (%gc-read64 (%ar-deadline-addr i))))
        (when (> d 0)
          (if (and (<= d now) (= (actor-get i #x00) 4))
              (progn (%gc-write64 (%ar-deadline-addr i) 0)
                     (%gc-write64 (%ar-timedout-addr i) 1)
                     (actor-set i #x00 2)
                     (actor-enqueue i))
              (when (or (zerop soonest) (< d soonest)) (setq soonest d)))))
      (setq i (+ i 1)))
    soonest))

(defun %ar-idle-wait (b g soonest)
  "%SCHED-IDLE-WAIT, but no longer than until SOONEST (0 = no limit)."
  (%gc-write64 (+ b #x40) (+ (%gc-read64 (+ b #x40)) 1))
  (if (zerop (%gc-read64 g))
      (if (zerop soonest)
          (%futex-wait g 0)
          (let ((ms (max 1 (ceiling (- soonest (%monotonic-ns)) 1000000))))
            (%futex-wait-to g 0 (%cond-ts ms))))
      0))

(defun %ar-sched-run ()
  "A scheduler thread's loop: %SCHED-RUN's protocol (declare the intent to
   sleep, then look at the queue, then FUTEX_WAIT), plus the two halves of an
   actor switch.  An actor hands the thread back by RESTORE-CONTEXT into the
   save area this loop rewrites every iteration; it resumes just after the
   SAVE-CONTEXT with FS still on the actor's window — so the first thing done
   is to put FS back."
  (let ((b (%sched-block))
        (g (%sched-globals)))
    (if (or (zerop b) (zerop g))
        0
        (progn
          (set-current-actor 0)
          (%gc-write64 (+ b #x60) 1)
          (loop
            (when (%sched-stop-p) (return 0))
            (save-context b)
            (%ar-set-fs (%ar-thread-delta))
            (when (%sched-stop-p) (return 0))
            (xchg-mem g 0)
            (spin-lock (sched-lock-addr))
            (let* ((soonest (%ar-expire))
                   (id (actor-dequeue)))
              (if (zerop id)
                  (progn
                    (spin-unlock (sched-lock-addr))
                    (set-idle-flag 1)
                    (%ar-idle-wait b g soonest)
                    (set-idle-flag 0))
                  (progn
                    (set-idle-flag 0)
                    (set-current-actor id)
                    (actor-set id #x00 1)
                    (%ar-set-fs (- (%ar-window id) #x10000000))
                    (actor-region-resume id)
                    (restore-context (+ (actor-struct-addr id) #x08))))))
          (%gc-write64 (+ b #x60) 0)
          0))))

(defun %ar-hand-back (id)
  "Give this thread back to its scheduler from actor ID, WITH THE SCHEDULER
   LOCK HELD (RESTORE-CONTEXT releases it after the stack switch).  The actor's
   context was saved by the caller; its region parks at that SP and the
   thread's own region becomes the running one."
  (%ar-mark-parked id (* (actor-get id #x08) 2))
  (%gc-region-switch (%ar-thread-rcb) (* (actor-get id #x08) 2))
  (set-current-actor 0)
  (restore-context (%sched-block)))

;;; ------------------------------------------------------------
;;; ACTOR LIFECYCLE
;;; ------------------------------------------------------------

(defun %ar-actor-entry ()
  "Where every actor starts.  Runs its function inside its own computation
   state and a handler for anything it does not handle, then ends."
  (let ((id (get-current-actor)))
    (%ar-mark-running id)
    (%with-computation-state ((* (+ id 16) 1099511627776))
      (let ((*ar-deferred* nil))
        (declare (special *ar-deferred*))
        (%gc-write64 (%ar-deadline-addr id) 0)
        (handler-case (funcall (svref *ar-fns* id))
          (serious-condition (e) (%ar-report id e)))))
    (%ar-exit id)))

(defun %ar-entry-addr ()
  (- (%gc-word-of (fn-addr %ar-actor-entry) (+ *ar-ctl* #x50)) 3))

(defun %ar-report (id e)
  (let ((w (+ (%tls-self-base) #x100050B0))
        (m (ignore-errors (princ-to-string e))))
    (dolist (str (list (format nil "~%modus: actor ~D ended with an unhandled error: " id)
                       (if (stringp m) m "an error")
                       (string #\Newline)))
      (dotimes (i (length str))
        (%gc-write64 w (logand (char-code (char str i)) 255))
        (syscall3 1 2 w 1)))))

(defun %ar-exit (id)
  "Actor ID is done: tell its link, free its function slot, mark it dead and
   hand the thread back.  The slot is reusable once the switch is complete —
   ACTORS-SPAWN claims dead slots under the same lock this holds for the
   switch."
  (let ((linked (actor-get id #x60)))
    (unless (zerop linked) (actors-send linked (list :exit id))))
  (setf (svref *ar-fns* id) nil)
  (spin-lock (sched-lock-addr))
  (actor-set id #x00 3)
  (%ar-mark-running id)
  (set-current-actor 0)
  (%gc-region-enter (%ar-thread-rcb))
  (restore-context (%sched-block)))

(defun %ar-clear-region-bits (id)
  "Clear the collector's object-start and cons-kind bits over both of actor
   ID's semispaces.  A REUSED SLOT MUST START WITH NO BITS SET.  The stack scan
   is conservative: a word counts as a root when the object-start bitmap says an
   object begins where it points.  The previous occupant's bits outlive it, and
   the new actor's live frames can hold uninitialised words left on the stack
   by the old one -- a stale word at a stale bit was taken for a root, and
   evacuating it wrote a forwarding pointer into the middle of a live object
   (a fresh slot's stack and bits are zero, so only reuse corrupted).  Regions
   are 1024-byte aligned, so each region's bits fill whole bitmap words and
   clearing them cannot touch a neighbour's."
  (let ((pb (%gc-bitmap-page-base-exact))
        (bb (%gc-read64 (%conv-addr #x10000E18)))
        (n (ceiling (%ar-get #x10) 128)))
    (unless (zerop bb)
      (dolist (space (list (%ar-region-from id) (%ar-region-to id)))
        (let ((b (+ bb (floor (- space pb) 128))))
          (%ha-zero b (+ b n))
          (%ha-zero (+ b #xFE4000) (+ b #xFE4000 n)))))
    0))

(defun actors-spawn (fn)
  "Start an actor running FN (a function of no arguments; from inside an actor
   pass a SYMBOL naming one).  Returns its id, or signals when every slot is in
   use."
  (unless (%ar-started-p) (error "actors-spawn: the actor runtime is not started (ACTORS-START)."))
  (spin-lock (sched-lock-addr))
  (let ((id 0) (i 2) (n (+ (%ar-get #x08) 2)))
    (loop
      (when (>= i n) (return 0))
      (let ((st (actor-get i #x00)))
        (when (or (= st 0) (= st 3)) (setq id i) (return 0)))
      (setq i (+ i 1)))
    (if (zerop id)
        (progn (spin-unlock (sched-lock-addr))
               (error "actors-spawn: all ~D actor slots are in use." (%ar-get #x08)))
        (progn
          (setf (svref *ar-fns* id) fn)
          (dolist (off '(#x08 #x10 #x18 #x20 #x28 #x30 #x40 #x48 #x50 #x58 #x60 #x70 #x78))
            (actor-set id off 0))
          (actor-set id #x38 id)
          (actor-set id #x08 (untag (%ar-stack-top id)))
          (actor-set id #x30 (untag (%ar-entry-addr)))
          (%gc-region-init (%ar-rcb id) (%ar-region-from id) (%ar-region-to id)
                           (%ar-get #x10) (%ar-stack-top id) (%gc-meta-scale))
          (%ar-clear-region-bits id)
          (actor-set id #x68 (untag (%ar-rcb id)))
          (%ar-prepare-window id)
          (%ar-mark-parked id (%ar-stack-top id))
          (%gc-write64 (%ar-staging id) 0)
          (%gc-write64 (+ (%ar-staging id) 8) 0)
          (actor-set id #x00 2)
          (actor-enqueue id)
          (spin-unlock (sched-lock-addr))
          (wake-idle-ap)
          id))))

;;; ------------------------------------------------------------
;;; MESSAGES
;;; ------------------------------------------------------------
;;;
;;; A MESSAGE IS BYTES.  The sender serialises it into a byte vector of its
;;; own (outside any lock: an error there -- an unsendable object -- strands
;;; nothing), then, under the scheduler lock, copies the bytes into the
;;; recipient's staging area (a loop of byte stores that cannot fail) and
;;; enqueues a mailbox cell holding the offset.  The recipient decodes them
;;; into ITS OWN heap.  No pointer ever crosses: that is the whole soundness
;;; argument of per-actor heaps, and net/actors.lisp's TERM-ENCODE broke it
;;; (an unknown type went across as its own address) and could not encode a
;;; proper list on hosted x86-64 at all (it tested the NIL terminator with
;;; ZEROP).
;;;
;;; ENCODING (one tag byte, then):
;;;   0 NIL   1 T   2 fixnum: sign byte, 8-byte LE magnitude
;;;   3 other number: its printed form (tag-12 string)   4 character: u32
;;;   5 string of codes < 256: u32 n, n bytes     6 string: u32 n, n x u32
;;;   7 symbol: package name, name (strings; package "" = uninterned)
;;;   8 list: u32 n, n elements, then the tail
;;;   9 hash table: test symbol, u32 n, n key/value pairs
;;;   10 (unsigned-byte 8) vector: u32 n, n bytes
;;;   11 general vector / struct / CLOS instance: u32 n, n elements (a struct
;;;      or instance carries its type or class NAME as a symbol in slot 1, so
;;;      the receiver rebuilds the same kind of object)
;;;   12 / 13 the struct / CLOS-instance marker (the image's own symbols)
;;; Anything else -- a function, a closure, a stream -- is refused, loudly.
;;;
;;; STAGING, per actor, 256 KB: +0 write offset, +8 messages outstanding,
;;; data from +16.  Messages are consumed in order, so the area empties when
;;; the outstanding count reaches zero and the write offset rewinds then.  A
;;; message that does not fit waits (backpressure) and, after 10 s, signals.

(defun %ar-staging-bytes () #x40000)
(defun %ar-staging (id) (+ (%ar-get #x58) (* id (%ar-staging-bytes))))

(defun %ar-b (buf pos v)
  (when buf (setf (aref buf pos) v))
  (+ pos 1))

(defun %ar-u32 (buf pos v)
  (%ar-b buf (%ar-b buf (%ar-b buf (%ar-b buf pos (logand v 255))
                               (logand (ash v -8) 255))
                    (logand (ash v -16) 255))
         (logand (ash v -24) 255)))

(defun %ar-str (buf pos str)
  (let ((n (length str)) (narrow t))
    (dotimes (i n) (when (> (char-code (char str i)) 255) (setq narrow nil)))
    (if narrow
        (let ((p (%ar-u32 buf (%ar-b buf pos 5) n)))
          (dotimes (i n) (setq p (%ar-b buf p (char-code (char str i)))))
          p)
        (let ((p (%ar-u32 buf (%ar-b buf pos 6) n)))
          (dotimes (i n) (setq p (%ar-u32 buf p (char-code (char str i)))))
          p))))

(defun %ar-emit (x buf pos depth)
  "Encode X at POS in BUF (NIL: only count).  Returns the next position."
  (when (> depth 3000)
    (error "actors: a message nested more than 3000 deep (or circular) cannot be sent."))
  (cond
    ((null x) (%ar-b buf pos 0))
    ((eq x t) (%ar-b buf pos 1))
    ((typep x 'fixnum)
     ;; sign byte + 8-byte magnitude: no 64-bit wrap arithmetic to get wrong
     (let ((v (if (< x 0) (- x) x))
           (p (%ar-b buf (%ar-b buf pos 2) (if (< x 0) 1 0))))
       (dotimes (i 8) (setq p (%ar-b buf p (logand (ash v (* -8 i)) 255))))
       p))
    ;; The struct and CLOS-instance MARKERS are the image's own symbols, and
    ;; interning the same name at run time yields a DIFFERENT symbol -- a
    ;; rebuilt instance would print right and fail TYPEP.  Sent as tags.
    ((eq x '%struct-instance) (%ar-b buf pos 12))
    ((eq x '%clos-instance) (%ar-b buf pos 13))
    ((numberp x) (%ar-str buf (%ar-b buf pos 3) (prin1-to-string x)))
    ((characterp x) (%ar-u32 buf (%ar-b buf pos 4) (char-code x)))
    ((stringp x) (%ar-str buf pos x))
    ((symbolp x)
     (let ((pk (symbol-package x)))
       (%ar-str buf (%ar-str buf (%ar-b buf pos 7) (if pk (package-name pk) ""))
                (symbol-name x))))
    ((hash-table-p x)
     (let ((p (%ar-u32 buf (%ar-emit (hash-table-test x) buf (%ar-b buf pos 9) (+ depth 1))
                       (hash-table-count x))))
       (maphash (lambda (k v)
                  (setq p (%ar-emit v buf (%ar-emit k buf p (+ depth 1)) (+ depth 1))))
                x)
       p))
    ((consp x)
     (let ((n 0) (q x) (slow x))
       (loop
         (unless (consp q) (return 0))
         (setq q (cdr q) n (+ n 1))
         (when (evenp n) (setq slow (cdr slow)))
         (when (and (consp q) (eq q slow))
           (error "actors: a circular list cannot be sent.")))
       (let ((p (%ar-u32 buf (%ar-b buf pos 8) n)) (q2 x))
         (dotimes (i n) (setq p (%ar-emit (car q2) buf p (+ depth 1)) q2 (cdr q2)))
         (%ar-emit q2 buf p (+ depth 1)))))
    ((functionp x)
     (error "actors: a function cannot be sent in a message -- send data, or a symbol naming the function."))
    ((= (obj-subtag x) 17)
     (let* ((n (length x)) (p (%ar-u32 buf (%ar-b buf pos 10) n)))
       (dotimes (i n) (setq p (%ar-b buf p (aref x i))))
       p))
    ((= (obj-subtag x) 50)
     (let* ((n (array-length x)) (p (%ar-u32 buf (%ar-b buf pos 11) n)))
       (dotimes (i n) (setq p (%ar-emit (aref x i) buf p (+ depth 1))))
       p))
    (t (error "actors: an object of type ~S cannot be sent in a message." (type-of x)))))

(defun %ar-encode (msg)
  (let* ((n (%ar-emit msg nil 0 0))
         (buf (make-array n :element-type '(unsigned-byte 8))))
    (%ar-emit msg buf 0 0)
    buf))

;;; Decoding reads raw staging memory through a one-slot cursor vector.

(defun %ar-rb (a cur)
  (let ((p (svref cur 0)))
    (setf (svref cur 0) (+ p 1))
    (mem-ref (+ a p) :u8)))

(defun %ar-ru32 (a cur)
  (let* ((b0 (%ar-rb a cur)) (b1 (%ar-rb a cur)) (b2 (%ar-rb a cur)) (b3 (%ar-rb a cur)))
    (+ b0 (ash b1 8) (ash b2 16) (ash b3 24))))

(defun %ar-rstr (a cur)
  (let* ((tag (%ar-rb a cur)) (n (%ar-ru32 a cur)) (s (make-string n)))
    (dotimes (i n)
      (setf (char s i) (code-char (if (= tag 5) (%ar-rb a cur) (%ar-ru32 a cur)))))
    s))

(defun %ar-decode (a cur)
  (let ((tag (%ar-rb a cur)))
    (cond
      ((= tag 0) nil)
      ((= tag 1) t)
      ((= tag 2)
       (let ((neg (= (%ar-rb a cur) 1)) (v 0))
         (dotimes (i 8) (setq v (+ v (ash (%ar-rb a cur) (* 8 i)))))
         (if neg (- v) v)))
      ((= tag 12) '%struct-instance)
      ((= tag 13) '%clos-instance)
      ((= tag 3) (let ((*read-eval* nil)) (read-from-string (%ar-rstr a cur))))
      ((= tag 4) (code-char (%ar-ru32 a cur)))
      ((or (= tag 5) (= tag 6))
       (setf (svref cur 0) (- (svref cur 0) 1))
       (%ar-rstr a cur))
      ((= tag 7)
       (let* ((pk (%ar-rstr a cur)) (nm (%ar-rstr a cur)))
         (if (zerop (length pk))
             (make-symbol nm)
             (let ((p (find-package pk)))
               (unless p (error "actors: a message names package ~A, which does not exist here." pk))
               (intern nm p)))))
      ((= tag 8)
       (let* ((n (%ar-ru32 a cur)) (head (cons nil nil)) (tail head))
         (dotimes (i n)
           (let ((c (cons (%ar-decode a cur) nil)))
             (setf (cdr tail) c tail c)))
         (setf (cdr tail) (%ar-decode a cur))
         (cdr head)))
      ((= tag 9)
       (let* ((test (%ar-decode a cur)) (n (%ar-ru32 a cur))
              (h (make-hash-table :test test)))
         (dotimes (i n)
           (let* ((k (%ar-decode a cur)) (v (%ar-decode a cur)))
             (setf (gethash k h) v)))
         h))
      ((= tag 10)
       (let* ((n (%ar-ru32 a cur)) (v (make-array n :element-type '(unsigned-byte 8))))
         (dotimes (i n) (setf (aref v i) (%ar-rb a cur)))
         v))
      ((= tag 11)
       (let* ((n (%ar-ru32 a cur)) (v (make-array n)))
         (dotimes (i n) (setf (aref v i) (%ar-decode a cur)))
         v))
      (t (error "actors: corrupt message (tag ~D)." tag)))))

(defun actors-self () (get-current-actor))

(defun %ar-post (target bytes)
  "Under the lock: copy BYTES into TARGET's staging and enqueue the cell.
   1 = posted (the lock is released by MAILBOX-ENQUEUE-AND-WAKE), 0 = no room
   yet (lock released here).  Nothing in here can signal."
  (spin-lock (sched-lock-addr))
  (let* ((sb (%ar-staging target))
         (w (%gc-read64 sb))
         (n (length bytes)))
    (if (> (+ w n) (- (%ar-staging-bytes) 16))
        (progn (spin-unlock (sched-lock-addr)) 0)
        (let ((cell (pool-alloc)))
          (if (zerop cell)
              (progn (spin-unlock (sched-lock-addr)) 0)
              (let ((d (+ sb (+ 16 w))))
                (dotimes (i n) (setf (mem-ref (+ d i) :u8) (aref bytes i)))
                (%gc-write64 sb (+ w n))
                (%gc-write64 (+ sb 8) (+ (%gc-read64 (+ sb 8)) 1))
                (set-car cell w)
                (set-cdr cell 0)
                (mailbox-enqueue-and-wake target cell)
                1))))))

(defun actors-send (target message)
  "Send MESSAGE (data: see the encoding above) to actor TARGET.  Returns
   MESSAGE.  Waits while TARGET's staging is full; signals after 10 s."
  (let ((bytes (%ar-encode message)))
    (when (> (length bytes) (- (%ar-staging-bytes) 16))
      (error "actors-send: the message is ~D bytes; at most ~D fit."
             (length bytes) (- (%ar-staging-bytes) 16)))
    (let ((deadline (+ (%monotonic-ns) 10000000000)))
      (loop
        (unless (zerop (%ar-post target bytes)) (return 0))
        (when (> (%monotonic-ns) deadline)
          (error "actors-send: actor ~D has not drained its mailbox for 10 s." target))
        (if (>= (get-current-actor) 2) (actors-yield) (%sleep-ms 1))))
    (when (= target 1)
      (xchg-mem (+ *ar-ctl* #x48) 1)
      (%futex-wake-all (+ *ar-ctl* #x48)))
    message))

(defun %ar-dequeue (id)
  "Under the lock: the next cell's staging offset + 1, or 0 when empty."
  (let ((head (actor-get id #x50)))
    (if (zerop head)
        0
        (let ((off (car head)) (next (cdr head)))
          (actor-set id #x50 next)
          (if (zerop next) (actor-set id #x58 0) 0)
          (pool-free head)
          (+ off 1)))))

(defun %ar-take (id off1)
  "Decode the message at OFF1-1 of ID's staging into this heap, then release
   its bytes (rewinding the area when nothing is outstanding)."
  (let* ((sb (%ar-staging id))
         (m (%ar-decode (+ sb 16) (vector (- off1 1)))))
    (spin-lock (sched-lock-addr))
    (let ((out (- (%gc-read64 (+ sb 8)) 1)))
      (%gc-write64 (+ sb 8) out)
      (when (zerop out) (%gc-write64 sb 0)))
    (spin-unlock (sched-lock-addr))
    m))

(defvar *ar-deferred* nil
  "Messages ACTORS-RECEIVE-IF set aside, oldest first.  Bound per actor (in
   its entry), so it lives in the actor's own heap; main's is the global.")

(defun %ar-raw-receive (timeout-ms)
  "The next mailbox message: (values message t), or (values nil nil) when
   TIMEOUT-MS (NIL = wait forever) passes first."
  (let* ((id (get-current-actor))
         (deadline (if timeout-ms (+ (%monotonic-ns) (* timeout-ms 1000000)) 0)))
    (if (= id 1)
        (loop
          (xchg-mem (+ *ar-ctl* #x48) 0)
          (spin-lock (sched-lock-addr))
          (let ((o (%ar-dequeue 1)))
            (spin-unlock (sched-lock-addr))
            (unless (zerop o) (return (values (%ar-take 1 o) t))))
          (if (zerop deadline)
              (%futex-wait (+ *ar-ctl* #x48) 0)
              (let ((left (- deadline (%monotonic-ns))))
                (when (<= left 0) (return (values nil nil)))
                (%futex-wait-to (+ *ar-ctl* #x48) 0
                                (%cond-ts (max 1 (ceiling left 1000000)))))))
        (progn
          (%gc-write64 (%ar-timedout-addr id) 0)
          (loop
            (%ar-mark-running id)
            (spin-lock (sched-lock-addr))
            (let ((o (%ar-dequeue id)))
              (cond
                ((not (zerop o))
                 (%gc-write64 (%ar-deadline-addr id) 0)
                 (spin-unlock (sched-lock-addr))
                 (return (values (%ar-take id o) t)))
                ((and (> deadline 0)
                      (or (= (%gc-read64 (%ar-timedout-addr id)) 1)
                          (>= (%monotonic-ns) deadline)))
                 (%gc-write64 (%ar-deadline-addr id) 0)
                 (%gc-write64 (%ar-timedout-addr id) 0)
                 (spin-unlock (sched-lock-addr))
                 (return (values nil nil)))
                (t
                 (when (zerop (save-context (+ (actor-struct-addr id) #x08)))
                   (%gc-write64 (%ar-deadline-addr id) deadline)
                   (actor-set id #x00 4)
                   (%ar-hand-back id))))))))))

(defun actors-receive (&optional timeout-ms)
  "The next message for this actor: (values message t), or (values nil nil)
   if TIMEOUT-MS milliseconds pass first (NIL, the default, waits forever).
   Messages set aside by ACTORS-RECEIVE-IF come first, in order.  From an
   actor the thread goes back to its scheduler while it waits; from the main
   thread (actor 1) it sleeps on the main wake word."
  (if *ar-deferred*
      (values (pop *ar-deferred*) t)
      (%ar-raw-receive timeout-ms)))

(defun actors-receive-if (predicate &optional timeout-ms)
  "The first message satisfying PREDICATE (selective receive): (values message
   t), or (values nil nil) on timeout.  Messages that do not match are kept,
   in order, for later receives."
  (let ((prev nil) (q *ar-deferred*))
    (loop
      (unless q (return 0))
      (when (funcall predicate (car q))
        (if prev (setf (cdr prev) (cdr q)) (setq *ar-deferred* (cdr q)))
        (return-from actors-receive-if (values (car q) t)))
      (setq prev q q (cdr q))))
  (let ((deadline (if timeout-ms (+ (%monotonic-ns) (* timeout-ms 1000000)) 0)))
    (loop
      (let ((left (if (zerop deadline) nil (max 0 (ceiling (- deadline (%monotonic-ns)) 1000000)))))
        (multiple-value-bind (m got) (%ar-raw-receive left)
          (unless got (return (values nil nil)))
          (if (funcall predicate m)
              (return (values m t))
              (setq *ar-deferred* (append *ar-deferred* (list m)))))))))

(defun actors-yield ()
  (let ((id (get-current-actor)))
    (if (< id 2)
        0
        (progn
          (spin-lock (sched-lock-addr))
          (if (zerop (save-context (+ (actor-struct-addr id) #x08)))
              (progn (actor-set id #x00 2)
                     (actor-enqueue id)
                     (%ar-hand-back id))
              (%ar-mark-running id))))))

(defun actors-link (a b)
  (actor-set a #x60 b)
  (actor-set b #x60 a)
  0)

;;; ------------------------------------------------------------
;;; START / STOP
;;; ------------------------------------------------------------

(defun actors-start (nthreads nslots region-bytes)
  "Bring the actor runtime up: threads on, NSLOTS actor slots each with a
   REGION-BYTES semispace pair, and NTHREADS scheduler threads.  Once per
   process.  Returns T, or signals why not."
  (when (%ar-started-p) (return-from actors-start t))
  (when (> nslots (%ar-max-slots)) (error "actors-start: at most ~D slots." (%ar-max-slots)))
  (when (zerop (%ha-carve)) (error "actors-start: the actor band could not be carved."))
  (%ha-percpu-init-cpu (%ha-percpu-base) 0)
  (%ha-set-percpu-mode 1)
  (%rt-threads-on)
  (when (zerop (%rt-threads-live-p)) (error "actors-start: threads could not be turned on."))
  (let ((m (%mmap-shared-page #x4000)))
    (when (< m 4096) (error "actors-start: no control page."))
    (%ha-zero m (+ m #x4000))
    (setq *ar-ctl* m))
  (%ar-set #x08 nslots)
  ;; The parked-actor scan table, published to the collector last-written.
  (%gc-write64 #x100050D0 (+ *ar-ctl* #x1400))
  (let ((w (%mmap-shared-page (* 64 #x8000)))
        (d (%mmap-shared-page (* 64 #x4000)))
        (s (%mmap-shared-page (* 64 (%ar-stack-bytes)))))
    (when (or (< w 4096) (< d 4096) (< s 4096)) (error "actors-start: could not map windows/stacks."))
    ;; mprotect(slot bottom, 4096, PROT_NONE) for every slot: see %AR-STACK-BYTES.
    (dotimes (i 64) (syscall3 10 (+ s (* i (%ar-stack-bytes))) 4096 0))
    (%ar-set #x30 w) (%ar-set #x38 d) (%ar-set #x40 s))
  (let ((st (%mmap-shared-page (* 64 (%ar-staging-bytes)))))
    (when (< st 4096) (error "actors-start: could not map staging."))
    (%ar-set #x58 st))
  (when (zerop (%ar-carve nslots region-bytes))
    (error "actors-start: region 0 has no room for ~D actor regions of ~D bytes." nslots region-bytes))
  (setq *ar-fns* (make-array 64 :initial-element nil))
  ;; The actor system proper: per-CPU block, locks, actor table (main = 1),
  ;; mailbox pool, staging buffers, scheduler globals.
  (smp-init)
  (%ar-actor-init)
  (staging-init)
  (%sched-reset)
  (%ar-set #x28 nthreads)
  (dotimes (i nthreads)
    (let ((slot (%make-native-thread (lambda () (%ar-sched-run)))))
      (when (< slot 0) (error "actors-start: scheduler thread ~D did not start (~D)." i slot))
      (%ar-set (+ #x80 (* 8 i)) slot)))
  (%ar-set 0 1)
  t)

(defun %ar-actor-init ()
  "ACTOR-INIT without its serial banner: zero the table, set the scheduler
   state, make the calling thread actor 1, and start the mailbox pool."
  (%ha-zero (actor-table-base) (+ (actor-table-base) 8192))
  (let ((ss (sched-state-base)))
    (setf (mem-ref (+ ss 8) :u64) 2)
    (setf (mem-ref (+ ss #x10) :u64) 0)
    (setf (mem-ref (+ ss #x18) :u64) 0))
  (set-current-actor 1)
  (percpu-set 8 4000)
  (setf (mem-ref (+ (sched-state-base) #x18) :u64) 1)
  (pool-init)
  (actor-set 1 #x00 1)
  (actor-set 1 #x38 1)
  0)

(defun actors-stop ()
  "Stop the scheduler threads and wait for them.  Actors still blocked are
   abandoned with their threads."
  (when (%ar-started-p)
    (%sched-stop)
    (dotimes (i (%ar-get #x28))
      (%join-native-thread (%ar-get (+ #x80 (* 8 i))) 100000000))
    (%ar-set 0 2))
  t)
