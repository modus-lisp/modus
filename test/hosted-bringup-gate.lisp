;;;; hosted-bringup-gate.lisp — %SB-THREADS-UP LATCHES SUCCESS AND CLEARS THE
;;;; PER-CPU MODE WORD ON ITS WAY OUT.
;;;;
;;;;   ./modus --script test/hosted-bringup-gate.lisp
;;;;
;;;; Six probes, no threads spawned, no sockets, no glass.  Each prints its own
;;;; numbers; the last two are the finding.
;;;;
;;;; ============================================================
;;;; WHY THIS EXISTS
;;;; ============================================================
;;;;
;;;; test/glass-tx-cell.lisp prints the threads-live gate at every grade point
;;;; and it reads ZERO on the worker.  With the gate zero, %RT-ENTER is a
;;;; 32-bit load and a branch: the runtime-table lock never engages, every
;;;; SYMBOL-VALUE / INTERN / macro-table access on a worker runs unsynchronised,
;;;; and — because %RT-ARENA-CARVE is called INSIDE %RT-THREADS-ON, past its
;;;; early return — B-lite's arena is never carved either.
;;;;
;;;; That last one matters beyond this file: the glass wall was measured
;;;; BYTE-IDENTICAL before and after B-lite and treated as evidence that the
;;;; fix had missed it.  A likelier reading is that the fix was never switched
;;;; on in that process.  (NOT yet demonstrated — see WHAT IS NOT SHOWN.)
;;;;
;;;; ============================================================
;;;; WHAT IS MEASURED — AND ONE PROBE PER PROCESS, OR IT MEASURES NOTHING
;;;; ============================================================
;;;;
;;;; ***THE FIRST VERSION OF THIS FILE RAN PROBE A FIRST, UNCONDITIONALLY.***
;;;; Probe A spawns a thread, spawning a thread ARMS the gate, and B/C/D/E then
;;;; ran on an already-armed system and reported a healthy one.  Every number in
;;;; the first draft of this header was that artifact.  A is gated now; run
;;;; exactly one probe per process or read nothing.
;;;;
;;;; A  MODUS_BRINGUP_PROBE=A — a bare MAKE-THREAD ARMS the gate: the worker
;;;;    reads gate=1 mode=1.  So "MAKE-THREAD does not arm the gate" is NOT true
;;;;    as a blanket statement.
;;;;
;;;; B  MODUS_BRINGUP_PROBE=B — %SB-THREADS-UP alone, IN THIS FILE'S SHAPE:
;;;;    pre gate=0 mode=0 flag=NIL -> post ret=T gate=1 mode=1 flag=T.  It ARMS.
;;;;
;;;; F  test/hosted-bringup-bare.lisp — THE SAME CALL, IN A BARE SCRIPT
;;;;    (two FORMATs and a LET, no DEFUNs, no COND): pre gate=0 mode=0 flag=NIL
;;;;    -> post **ret=T gate=0 mode=0 flag=T**, 4 of 4, with and without a large
;;;;    DEFVAR prepended.
;;;;
;;;; ***B AND F ARE THE SAME CALL AND DISAGREE.***  That is the finding, and it
;;;; is the campaign's layout-sensitivity again rather than a mechanism:
;;;; %SB-THREADS-UP arms the seam in some script shapes and, in others, RETURNS
;;;; T AND LATCHES *SB-THREADS-UP* WITHOUT ARMING ANYTHING.  Once latched, every
;;;; later call short-circuits on the flag, so the seam can never come up in
;;;; that process.  The latch is unconditional:
;;;;
;;;;      (progn (%ha-percpu-init-cpu (%ha-percpu-base) 0)
;;;;             (%ha-set-percpu-mode 1)
;;;;             (%rt-threads-on)          ; <- return value DISCARDED
;;;;             (setq *sb-threads-up* t)
;;;;             t)                        ; <- unconditional T
;;;;
;;;;    so an unarmed bringup is indistinguishable from an armed one to every
;;;;    caller.  THAT is the defect this file establishes, whatever turns out to
;;;;    decide which way a given shape falls.
;;;;
;;;; C  MODUS_BRINGUP_PROBE=C — the four steps of the body, at toplevel: carve
;;;;    non-zero, percpu-init 0, set-mode -> mode=1, rt-threads-on -> 1.  (Its
;;;;    gate readback prints 0 because it is read in the SAME FORMAT call that
;;;;    invokes %RT-THREADS-ON; read it in a separate form and it is 1.  Another
;;;;    instrument artifact — do not read C's gate column.)
;;;;
;;;; D  MODUS_BRINGUP_PROBE=D — set mode to 1 BY HAND, then call it.
;;;;    ***A PREVIOUS ROUND REPORTED "mode=1 in, mode=0 out — %SB-THREADS-UP
;;;;    CLEARS THE MODE WORD".  THAT DOES NOT REPRODUCE.***  One probe per
;;;;    process it reads mode=1 gate=1: mode survives.  The clearing was a
;;;;    property of that harness, not of the tree.  D prints its own verdict
;;;;    line; believe the line, not this paragraph.
;;;;
;;;; E  MODUS_BRINGUP_PROBE=E — %HA-PERCPU-INIT-CPU with mode pre-set: mode
;;;;    survives it.  So percpu-init is NOT a clearing call.
;;;;
;;;; ============================================================
;;;; WHY IT MATTERS, AND WHAT IS NOT SHOWN
;;;; ============================================================
;;;;
;;;; test/glass-tx-cell.lisp prints the gate at every grade point and it reads
;;;; ZERO on the worker — glass's process is in the non-arming shape.  With the
;;;; gate zero, %RT-ENTER is a 32-bit load and a branch: the runtime-table lock
;;;; never engages, every SYMBOL-VALUE / INTERN / macro-table access on a worker
;;;; runs unsynchronised, and — because %RT-ARENA-CARVE is called INSIDE
;;;; %RT-THREADS-ON, past its early return — B-lite's arena is never carved.
;;;;
;;;; That last one is a candidate explanation for the glass wall having measured
;;;; BYTE-IDENTICAL before and after B-lite: not that the fix missed it, but
;;;; that the fix was never switched on there.  ***CANDIDATE, NOT SHOWN.***
;;;;
;;;;   NOT SHOWN: what decides which shape arms.  Both B and F are two FORMATs
;;;;   and a LET around one call; the differences are DEFUNs and a COND.
;;;;
;;;;   NOT SHOWN: whether arming the gate changes the tx-cell overwrite at all.
;;;;   Untested.  If the overwrite is independent of the lock it survives arming
;;;;   and the writer is still unfound.
;;;;
;;;;   NOT SHOWN: whether this is the 327680-byte shim-audit regression, which
;;;;   discriminated on "arena binaries".  If the arena never engaged there, the
;;;;   discriminant was something else.  Suggestive, unmeasured.
;;;;
;;;; WHAT TO DO ABOUT IT is not in this file: a bringup that cannot arm the seam
;;;; should SIGNAL, and should not latch *SB-THREADS-UP* on a partial success.
;;;; That is a source change to net/sb-thread-shim.lisp and a rebuild, and it
;;;; was not attempted here.
;;;;
;;;; NOTHING HERE LISTENS, CONNECTS, OR LEAVES A THREAD RUNNING: probe A joins
;;;; its one thread; the rest is memory reads.

(defun bg-gate () (mem-ref #x10000DB8 :u32))
(defun bg-mode () (mem-ref #x10000FF8 :u32))

(format t "~&=== hosted-bringup-gate ===~%")
(format t "~&start: gate=~s mode=~s flag=~s~%" (bg-gate) (bg-mode) *sb-threads-up*)
(finish-output)

;;; ---- A: does a bare MAKE-THREAD arm it? -------------------------------------
;;; A IS ITS OWN PROBE AND MUST NOT RUN BEFORE THE OTHERS.  Spawning the thread
;;; ARMS the gate for the rest of the process, so running A first made B, C, D
;;; and E all report a healthy, already-armed system and answer nothing.  That
;;; is the harness invalidating its own experiment — one process, one probe.
(when (let ((w (%cli-getenv "MODUS_BRINGUP_PROBE"))) (or (null w) (string= w "A")))
  (let ((th (sb-thread:make-thread
             (lambda () (list :gate (bg-gate) :mode (bg-mode)))
             :name "bringup-probe")))
    (format t "~&A make-thread: worker saw ~s ; main now gate=~s mode=~s flag=~s~%"
            (sb-thread:join-thread th) (bg-gate) (bg-mode) *sb-threads-up*)))
(finish-output)

(format t "~&~%--- the probes below need a FRESH process each; run with~%~
             --- MODUS_BRINGUP_PROBE=B|C|D|E to pick one.  Default: report only.~%")

(let ((which (%cli-getenv "MODUS_BRINGUP_PROBE")))
  (cond
    ;; ---- B: %SB-THREADS-UP alone ------------------------------------------
    ((and which (string= which "B"))
     (format t "~&B pre : gate=~s mode=~s flag=~s~%" (bg-gate) (bg-mode) *sb-threads-up*)
     (let ((r (%sb-threads-up)))
       (format t "~&B post: returned ~s gate=~s mode=~s flag=~s~%"
               r (bg-gate) (bg-mode) *sb-threads-up*)))

    ;; ---- C: the four steps, by hand ---------------------------------------
    ((and which (string= which "C"))
     (format t "~&C0 mode=~s gate=~s~%" (bg-mode) (bg-gate))
     (format t "~&C1 carve=~s mode=~s~%" (%ha-carve) (bg-mode))
     (format t "~&C2 percpu-init=~s mode=~s~%"
             (%ha-percpu-init-cpu (%ha-percpu-base) 0) (bg-mode))
     (format t "~&C3 set-mode=~s mode=~s~%" (%ha-set-percpu-mode 1) (bg-mode))
     (format t "~&C4 rt-threads-on=~s mode=~s gate=~s~%"
             (%rt-threads-on) (bg-mode) (bg-gate)))

    ;; ---- D: THE FINDING — mode 1 in, mode 0 out ---------------------------
    ((and which (string= which "D"))
     (%ha-carve)
     (%ha-percpu-init-cpu (%ha-percpu-base) 0)
     (%ha-set-percpu-mode 1)
     (format t "~&D PRE  mode=~s gate=~s flag=~s~%" (bg-mode) (bg-gate) *sb-threads-up*)
     (let ((r (%sb-threads-up)))
       (format t "~&D POST sb-threads-up=~s mode=~s gate=~s flag=~s~%"
               r (bg-mode) (bg-gate) *sb-threads-up*))
     (format t "~&D VERDICT: ~a~%"
             (if (zerop (bg-mode))
                 "MODE WAS CLEARED by %SB-THREADS-UP (1 in, 0 out)"
                 "mode survived — the finding did NOT reproduce, report that")))

    ;; ---- E: which call clears it? -----------------------------------------
    ((and which (string= which "E"))
     (%ha-carve)
     (%ha-set-percpu-mode 1)
     (format t "~&E pre-percpu-init      mode=~s~%" (bg-mode))
     (let ((r (%ha-percpu-init-cpu (%ha-percpu-base) 0)))
       (format t "~&E percpu-init=~s      mode=~s~%" r (bg-mode)))
     (%ha-set-percpu-mode 1)
     (format t "~&E re-set               mode=~s~%" (bg-mode))
     (let ((r (%rt-threads-on)))
       (format t "~&E rt-threads-on=~s    mode=~s gate=~s~%" r (bg-mode) (bg-gate)))
     (format t "~&E VERDICT: the clearing call is ~a~%"
             (if (zerop (bg-mode)) "STILL UNIDENTIFIED — mode is 0 at the end"
                 "not on this path — mode survived to the end")))

    (t (format t "~&(no probe selected)~%"))))

(finish-output)
(sys-exit 0)
