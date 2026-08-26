;;;; glass-tx-cell.lisp — THE WALL IS A HEAP OVERWRITE, AND THIS PRINTS IT.
;;;;
;;;;   test/run-glass-tx-cell.sh [MODUS-BINARY] [MODE] [GLASS-DIR] [CRAM-DIR]
;;;;
;;;; ============================================================
;;;; WHAT THIS SAYS THAT test/glass-send-worker.lisp DOES NOT
;;;; ============================================================
;;;;
;;;; run-glass-send-worker.sh reports "the `both' arm stops at 32785 of 49180
;;;; with MVM LONGJMP (TRAP #x0511) with no active handler-case".  Every word
;;;; of that is true and none of it names a defect: a longjmp complaint is what
;;;; the MVM interpreter prints (mvm/interp.lisp:898) when a TRAP #x0511 finds
;;;; an empty handler list, which is a SECOND-ORDER artefact of unwinding, not
;;;; the thing that went wrong.
;;;;
;;;; Arm the worker with a HANDLER-BIND and the real condition surfaces: a
;;;; TYPE-ERROR.  Grade GLASS::*TX* — the transfer counter that GLASS:W-U8
;;;; increments once per byte — between the two rectangles, and the TYPE-ERROR
;;;; has a name:
;;;;
;;;;     a=CONS/0   b=CONS/4   c=CONS/NOTINT:CHARACTER   c1=CONS/NOTINT:FUNCTION
;;;;
;;;; The cons is still a cons.  Its CAR was 0, then 4, and then it was A
;;;; CHARACTER, and then A FUNCTION.  Nothing in glass ever stores either into
;;;; it.  THE CELL IS BEING HANDED OUT AGAIN AS FRESH ALLOCATION MEMORY while
;;;; it is still live and still referenced.  `(incf (the fixnum (car *tx*)))'
;;;; in GLASS::TX+ then adds 1 to a character and signals the TYPE-ERROR, and
;;;; the unwind out of it is what prints the longjmp line.
;;;;
;;;; ============================================================
;;;; WHAT IT IS NOT — measured, each one killed by an arm below
;;;; ============================================================
;;;;
;;;; NOT THE UNWIND-PROTECT NESTING, which is what "the two wrappers, NESTED"
;;;; had pointed at for three rounds.  `with-fb-locked' is SB-THREAD:WITH-
;;;; RECURSIVE-LOCK (an unwind-protect) and a LET of a special compiles to
;;;; another (compile-let-with-specials), so `both' is two nested ones.  But
;;;; TX — ONE unwind-protect, no lock at all — corrupts the counter just as
;;;; `both' does, and BOTHNIL — the identical nesting to `both', differing only
;;;; in that *TX* is NIL so no counter cons is made — is CLEAN.  The nesting is
;;;; a passenger.
;;;;
;;;; NOT THE SPECIAL VARIABLE, and not a dynamic-binding stack: modus has no
;;;; binding stack to make per-thread.  COMPILE-LET-WITH-SPECIALS shallow-binds
;;;; — save the global cell into a LEXICAL slot, SET-SYMBOL-VALUE the new
;;;; value, restore from the lexical in an unwind-protect cleanup.  The save
;;;; already lives on the thread's own stack.  OTHERSP (the lock plus a LET of
;;;; *PRINT-BASE*, a special rebound over the same body) is CLEAN.
;;;;
;;;; NOT THE LOCK, in any sense: LOCK and BOTHNIL hold it and are clean, TX
;;;; does not hold it and is corrupted, and MYMTX — a mutex made in this file
;;;; and touched by nobody else — behaves as `both' does.
;;;;
;;;; NOT A RACE WITH MAIN.  GLASS_TX_MAINMODE=join parks main in JOIN-THREAD
;;;; instead of READ-BYTE, so main runs no reader and touches no stream:
;;;; `both' then fails IDENTICALLY — 32785 bytes, `c=CONS/NOTINT:CHARACTER'.
;;;; The worker is overwriting its OWN earlier allocation, and (%GC-REGION) on
;;;; the worker reads its own region block, not region 0's.
;;;;
;;;; NOT RECLAMATION OF AN UNROOTED OBJECT.  LOCKALLOC-KEEP holds the very same
;;;; cons in a LEXICAL on the worker's own stack for the whole extent of the
;;;; send — a live stack root, and it prints `keep=CONS' at the end to show the
;;;; root is still good — and the counter is overwritten anyway.
;;;;
;;;; ============================================================
;;;; WHAT IT IS, AND WHY EVERY EARLIER TABLE OF THIS WALL IS MISLEADING
;;;; ============================================================
;;;;
;;;; ***THE BYTE COUNT IS NOT THE ORACLE.  THE CAR IS.***  Grade the counter
;;;; instead of the transfer and the arms sort perfectly, ONE RUN EACH (the
;;;; first eight rows from a single `all' pass; lockset, mymtx and
;;;; lockalloc-keep run individually, same binary, same shape):
;;;;
;;;;   arm              counter installed?   CAR grade          delivered?
;;;;   plain            no  (*TX* NIL)       OFF, clean         yes
;;;;   lock             no                   OFF, clean         yes
;;;;   bothnil          no                   OFF, clean         yes
;;;;   othersp          no                   OFF, clean         yes
;;;;   tx               YES                  OVERWRITTEN        yes
;;;;   bothpre          YES                  OVERWRITTEN        yes
;;;;   lockset          YES                  OVERWRITTEN        yes
;;;;   lockalloc        YES                  OVERWRITTEN        yes
;;;;   lockalloc-keep   YES                  OVERWRITTEN        yes
;;;;   both             YES                  OVERWRITTEN        usually NOT
;;;;
;;;; EVERY arm that installs a live counter cons has that cons overwritten.
;;;; EVERY arm that installs none is clean.  Nothing else correlates — not the
;;;; lock, not the nesting, not which special, not where the cons was consed.
;;;;
;;;; SO WHAT `both' ACTUALLY IS is not a distinct failure: it is the ordinary
;;;; failure landing BEFORE the last TX+ that reads the counter instead of
;;;; after it.  That is why `both' stops at 32785 and the others do not, why
;;;; `both' is not even reliably 0-of-N in this shape, and why the fine
;;;; discriminants chased in a scratchpad reduction (LOCKALLOC 0 of 5 there,
;;;; 5 of 5 here) moved when the body was opened up: they were never measuring
;;;; whether the overwrite happened, only where it landed.
;;;;
;;;; ***AND test/run-glass-send-worker.sh's `tx' ARM IS NOT CLEAN EITHER.***  It
;;;; reports 49180 of 49180 and has for three rounds.  Its counter is corrupted
;;;; in the same run — here it read `c=CONS/26218438885942664', a garbage
;;;; FIXNUM rather than a character, so not even a TYPE-ERROR announces it.
;;;; That is the whole reason this file grades memory: a wire that carries every
;;;; byte proves nothing about a heap that has already been written over.
;;;;
;;;; The size dependence, measured in the reduction and NOT monotone — the tell
;;;; that this is placement, not a size limit.  GLASS_TX_FBW sets the width, so
;;;; rect 1's raw buffer is FBW*64*4 bytes:
;;;;
;;;;   FBW   8  buf  2048  DELIVERS      FBW  48  buf 12288  FAILS
;;;;   FBW  32  buf  8192  DELIVERS      FBW  96  buf 24576  FAILS
;;;;   FBW  64  buf 16384  DELIVERS      FBW 128  buf 32768  FAILS
;;;;
;;;; 4 pages deliver and 3 pages do not, so there is no threshold to find —
;;;; and by the table above those "DELIVERS" rows should be re-graded before
;;;; anyone leans on them.
;;;;
;;;; WHAT IS ESTABLISHED, in one sentence: a cons a worker allocates and keeps
;;;; writing to is handed out again as fresh allocation memory by that same
;;;; worker, deterministically, whenever glass's transfer counter is live.
;;;; WHAT IS NOT: which allocation writes it, or why.
;;;;
;;;; ============================================================
;;;; DEAD HYPOTHESIS: THE %RT-ENTER/%RT-LEAVE SEAM.  THE GATE READS ZERO.
;;;; ============================================================
;;;;
;;;; The natural next theory — and a good one, because it is the shape that was
;;;; really there in region 0 last round — is that the worker's OWN allocation
;;;; frontier is parked and reloaded across the runtime-table seam, and comes
;;;; back STALE: worker allocates the counter, keeps allocating in registers,
;;;; TX+ reads *TX* through SYMBOL-VALUE which parks the frontier and hops, and
;;;; %RT-LEAVE restores a value older than the registers had reached, so the
;;;; next allocation is issued over the counter.  Two copies of one frontier,
;;;; one level down.
;;;;
;;;; IT NEVER RUNS.  %RT-ENTER is
;;;;     (if (= (mem-ref #x10000DB8 :u32) 0) 0 (%rt-enter-locked))
;;;; and #x10000DB8 is the threads-live gate, a BSS word that ONLY
;;;; %RT-THREADS-ON writes.  SB-THREAD:MAKE-THREAD does not call it; nothing in
;;;; glass calls it; nothing in this file calls it.  CHK prints the gate at
;;;; every grade point, on the worker, and in the failing arm it reads:
;;;;
;;;;     a[gate=0]=CONS/0  b[gate=0]=CONS/4  c[gate=0]=CONS/NOTINT:CHARACTER
;;;;
;;;; Zero where the counter is still intact and zero where it is already
;;;; wrecked.  With the gate zero %RT-ENTER and %RT-LEAVE are a 32-bit load and
;;;; a branch: NO mutex, NO region hop, NO parked frontier, NOTHING to rewind.
;;;; The seam is not merely innocent here, it is not executed, so no
;;;; instrumentation of it can say anything.  Turning the gate ON would be a
;;;; different experiment about a different program.
;;;;
;;;; KEEP THE GATE PRINT.  It costs one number per grade point and it is what
;;;; stops this hypothesis being re-derived a fourth time.
;;;;
;;;; IT ALSO CORRECTS A CLAIM THIS CAMPAIGN HAS BEEN REPEATING.
;;;; test/glass-send-worker.lisp's header says a special read "compiles to
;;;; SYMBOL-VALUE, which takes the runtime-table lock and hops the active GC
;;;; region to region 0".  That is true only with the gate on, and the gate is
;;;; off in every run either test has ever made.  SYMBOL-VALUE here is a
;;;; GETHASH on the globals table and nothing else.
;;;;
;;;; AND IT NARROWS THE SUBJECT.  TX+ is `(when *tx* (incf (car *tx*)))', so
;;;; EVERY arm — including the clean ones — performs the SYMBOL-VALUE read once
;;;; per byte.  The reads are common to all arms and the clean arms are clean.
;;;; What only the corrupted arms do is WRITE: a cons exists and is RPLACA'd
;;;; ~32800 times while the worker allocates.
;;;;
;;;; ---- and one measurement that answered nothing, reported anyway ----
;;;; CANARY conses a 3-element list immediately before the counter and checks
;;;; it at the end.  It is INCONCLUSIVE: the canary is intact 3 of 3 — but so
;;;; is the counter, 0 of 3 overwritten, where the same arm without the canary
;;;; is corrupted.  ONE EXTRA THREE-CONS ALLOCATION MOVES THE BUG AWAY.  That
;;;; is the third independent demonstration of layout sensitivity in this file
;;;; (the others: opening BODY up, and the non-monotone FBW table), and it is
;;;; the standing warning for anyone instrumenting this — the probe changes the
;;;; thing it measures, so a clean run under a new probe is not evidence.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE GRADE IS OF MEMORY, NOT OF THE WIRE.  TYPE-OF on the CAR of a cons
;;;;   glass owns is not a self-referential check: no arm of this file, and no
;;;;   line of glass, ever stores a CHARACTER or a FUNCTION there.
;;;;
;;;;   THE PEER IS PYTHON AND IT COUNTS, so "it returned" is never the
;;;;   evidence; the byte count is taken on the far side of the kernel.
;;;;
;;;;   THE ARMS SHARE ONE BODY.  Every arm calls the same BODY on the same
;;;;   framebuffer over the same socket and differs only in the wrapper named
;;;;   in DO-SEND, so a difference between arms is that wrapper.
;;;;
;;;;   THE SCREEN IS TALLER THAN ONE BAND: 96 rows against *MAX-BAND-ROWS* = 64
;;;;   is two rectangles, and the corruption is visible between them.  A 64-row
;;;;   framebuffer passes every arm and proves nothing.
;;;;
;;;;   NOTHING IS LEFT LISTENING: loopback, a kernel-chosen port, 5900-5920
;;;;   refused outright, and `ss' asked afterwards by the runner.
;;;;
;;;; glass and cram are READ-ONLY: their sources are loaded where they sit.

;;; The manifest (test/glass-manifest.lisp) is prepended by the runner.
(unless (boundp '*glass-files*)
  (format t "~&test/glass-tx-cell.lisp needs a manifest; run its runner~%")
  (finish-output)
  (sys-exit 2))

(dolist (entry *glass-files*) (load (first entry)))
(format t "~&=== loaded ===~%")
(force-output)

(defun getenv-or (name default)
  (let ((s (%cli-getenv name)))
    (if (or (null s) (= (length s) 0)) default s)))

(defvar *mode* (getenv-or "GLASS_TX_MODE" "both"))
(defvar *mainmode* (getenv-or "GLASS_TX_MAINMODE" "read"))
(defvar *fbw* (let ((s (getenv-or "GLASS_TX_FBW" nil)))
                (if s (parse-integer s) 128)))
(defvar *fbh* 96)                       ; > 64 on purpose: glass bands at 64 rows
(defvar *mtx* (sb-thread:make-mutex :name "glass-tx-cell"))

(defun gsw-pixel (x y)
  (logior (ash (logand (* x 2) 255) 16)
          (ash (logand (* y 3) 255) 8)
          16))

;;; The bounds are LOCALS, not the globals: a special read compiles to
;;; SYMBOL-VALUE, which is a mechanism under test elsewhere and has no business
;;; running twelve thousand times inside the fixture that paints the reference.
(defun gsw-make-fb ()
  (let ((w *fbw*) (h *fbh*))
    (let ((fb (funcall (find-symbol "MAKE-FRAMEBUFFER" "GLASS") w h))
          (put (find-symbol "FB-PUT" "GLASS"))
          (y 0))
      (loop
        (when (>= y h) (return nil))
        (let ((x 0))
          (loop
            (when (>= x w) (return nil))
            (funcall put fb x y (gsw-pixel x y))
            (setq x (+ x 1))))
        (setq y (+ y 1)))
      fb)))

;;; ---- THE GRADE ---------------------------------------------------------------
;;;
;;; TAG=CONS/<n>            the counter is intact and holds <n>
;;; TAG=CONS/NOTINT:<type>  THE CELL WAS OVERWRITTEN — the CAR is a <type>
;;; TAG=BROKEN              *TX* itself is no longer a cons
;;;
;;; WRITE-STRING-SERIAL, not FORMAT: this runs on the worker at the exact
;;; moment under test, and the point is to disturb it as little as a print can.
(defun chk (tag)
  (write-string-serial tag)
  (write-string-serial "[gate=")
  (write-string-serial (princ-to-string (mem-ref #x10000DB8 :u32)))
  (write-string-serial "]")
  (if (null glass::*tx*)
      ;; The arms that never install a counter (plain / lock / bothnil /
      ;; othersp) legitimately read NIL.  That is OFF, not BROKEN — the runner
      ;; greps for BROKEN, so the two must not share a word.
      (write-string-serial "=OFF")
  (if (consp glass::*tx*)
      (progn
        (write-string-serial "=CONS/")
        (if (integerp (car glass::*tx*))
            (write-string-serial (princ-to-string (car glass::*tx*)))
            (progn (write-string-serial "NOTINT:")
                   (write-string-serial (princ-to-string (type-of (car glass::*tx*)))))))
      (write-string-serial "=BROKEN")))
  (write-string-serial " "))

;;; SEND-RECTS' own body, opened up so the counter can be graded BETWEEN the
;;; two rectangles.  Identical calls, identical order — GLASS:SEND-RECTS is
;;; (w-u8 0)(w-u8 0)(w-u16 nrects) then EMIT-RECT per banded rect, and with
;;; ENC 0 and no TRLE/ZRLE threshold met EMIT-RECT is WRITE-RECT-RAW.
(defun body (s fb)
  (let ((w-u8 (find-symbol "W-U8" "GLASS"))
        (w-u16 (find-symbol "W-U16" "GLASS"))
        (wrr (find-symbol "WRITE-RECT-RAW" "GLASS")))
    (chk "a")
    (funcall w-u8 s 0) (funcall w-u8 s 0) (funcall w-u16 s 2)
    (chk "b")
    (funcall wrr s fb 0 0 *fbw* 64 nil)
    (chk "c")
    (funcall wrr s fb 0 64 *fbw* 32 nil)
    (chk "d")
    (force-output s)
    (chk "e")))

;;; The arms.  ONE body; they differ only in the wrapper.
(defun do-send (s fb mode)
  (cond
    ;; ---- the four arms run-glass-send-worker.sh names ----
    ((string= mode "plain") (body s fb))
    ((string= mode "tx")    (let ((glass::*tx* (list 0))) (body s fb)))
    ((string= mode "lock")  (glass::with-fb-locked (fb) (body s fb)))
    ((string= mode "both")  (glass::with-fb-locked (fb)
                              (let ((glass::*tx* (list 0))) (body s fb))))
    ;; ---- the nesting is a passenger ----
    ;; identical nesting; *TX* NIL so TX+ reads the special and does no INCF
    ((string= mode "bothnil") (glass::with-fb-locked (fb)
                                (let ((glass::*tx* nil)) (body s fb))))
    ;; a NON-glass special under the same lock
    ((string= mode "othersp") (glass::with-fb-locked (fb)
                                (let ((*print-base* 10)) (body s fb))))
    ;; ---- where the cons is made is the whole difference ----
    ;; ONE unwind-protect, no special LET; cons made INSIDE the lock
    ((string= mode "lockalloc") (glass::with-fb-locked (fb)
                                  (setq glass::*tx* (list 0)) (body s fb)))
    ;; the same cons, made BEFORE grab-mutex
    ((string= mode "bothpre") (let ((c (list 0)))
                                (glass::with-fb-locked (fb)
                                  (let ((glass::*tx* c)) (body s fb)))))
    ;; cons before the lock, plain SETQ, counter fully live
    ((string= mode "lockset") (setq glass::*tx* (list 0))
                              (glass::with-fb-locked (fb) (body s fb)))
    ;; ---- it is not the framebuffer's lock ----
    ((string= mode "mymtx") (sb-thread:with-recursive-lock (*mtx*)
                              (let ((glass::*tx* (list 0))) (body s fb))))
    ;; ---- it is not lost rooting: KEEP is a live stack root throughout ----
    ((string= mode "lockalloc-keep")
     (glass::with-fb-locked (fb)
       (let ((keep (list 0)))
         (setq glass::*tx* keep)
         (body s fb)
         (write-string-serial (if (consp keep) "keep=CONS " "keep=BROKEN "))
         keep)))
    ;; ---- is it the counter, or is it ANY object allocated there? ----
    ;; CANARY conses a 3-element list next to the counter, hands it to nobody,
    ;; reads it from nobody until the end, and reports whether it still holds
    ;; (0 1 2).  If the canary rots too, the overwrite is indiscriminate and the
    ;; subject is the allocator, not *TX*.
    ((string= mode "canary")
     (glass::with-fb-locked (fb)
       (let ((canary (list 0 1 2)))
         (setq glass::*tx* (list 0))
         (body s fb)
         (write-string-serial
          (if (and (consp canary) (equal canary '(0 1 2)))
              "canary=INTACT "
              (concatenate 'string "canary=ROTTED:" (princ-to-string canary) " ")))
         canary)))
    (t (error "glass-tx-cell: unknown mode ~a" mode))))

(let* ((fb (gsw-make-fb))
       (listener (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
  (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
  (sb-bsd-sockets:socket-listen listener 4)
  (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name listener)
    (declare (ignore addr))
    (if (and (>= port 5900) (<= port 5920))
        (progn
          (format t "~&REFUSING: the kernel handed back ~d, inside 5900-5920.~%" port)
          (sb-bsd-sockets:socket-close listener)
          (finish-output)
          (sys-exit 1))
        nil)
    (format t "~&MODE ~a FBW ~d EXPECT ~d~%" *mode* *fbw* (+ 28 (* *fbw* 384)))
    (format t "~&PORT ~d~%" port)
    (finish-output))
  (let* ((conn (sb-bsd-sockets:socket-accept listener))
         (s (sb-bsd-sockets:socket-make-stream conn :input t :output t
                                                    :element-type '(unsigned-byte 8))))
    ;; THE HANDLER-BIND IS THE INSTRUMENT.  Without an armed handler above it
    ;; the TYPE-ERROR unwinds into the interpreter's empty handler list and is
    ;; reported as the longjmp line, losing its identity; with one, the arm
    ;; reports what actually signalled.
    (let ((th (sb-thread:make-thread
               (lambda ()
                 (write-string-serial "worker: sending ")
                 (handler-case (progn (do-send s fb *mode*)
                                      (write-string-serial "worker: sent ")
                                      :ok)
                   (error (e)
                     (write-string-serial "worker: CAUGHT ")
                     (write-string-serial (princ-to-string e))
                     (write-string-serial " ")
                     :err)))
               :name "glass-tx-cell")))
      ;; MAIN PARKS READING, as glass's reader thread does — or, with
      ;; GLASS_TX_MAINMODE=join, does not run a reader at all, which is the arm
      ;; that shows main is not the other mutator.
      (if (string= *mainmode* "read")
          (progn (format t "~&reader got ~s~%" (read-byte s nil :eof)) (finish-output))
          (format t "~&main: not reading~%"))
      (finish-output)
      (format t "~&worker joined: ~s~%" (sb-thread:join-thread th))
      (finish-output))
    (sb-bsd-sockets:socket-close conn))
  (sb-bsd-sockets:socket-close listener))

(format t "~&SERVER DONE~%")
(finish-output)
(sys-exit 0)
