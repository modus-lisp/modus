;;;; hosted-bringup-gate.lisp — %SB-THREADS-UP ARMS THE SEAM.  THE "IT DOESN'T"
;;;; REPORTS WERE AN INLINE-MEM-REF READ ARTIFACT, AND SO WERE BOTH MECHANISMS
;;;; BEFORE THEM.
;;;;
;;;;   ./modus --script test/hosted-bringup-gate.lisp
;;;;   MODUS_BRINGUP_PROBE=A|B|C|D|E|F|G ./modus --script test/hosted-bringup-gate.lisp
;;;;
;;;; ============================================================
;;;; ONE PROBE PER PROCESS, OR IT MEASURES NOTHING
;;;; ============================================================
;;;;
;;;; ***THE FIRST VERSION OF THIS FILE RAN PROBE A FIRST, UNCONDITIONALLY.***
;;;; Probe A spawns a thread, spawning a thread ARMS the seam, and B/C/D/E then
;;;; ran on an already-armed system and reported a healthy one.  Every number in
;;;; the first draft of this header was that artifact.  A is gated now; run
;;;; exactly one probe per process or read nothing.
;;;;
;;;; ============================================================
;;;; THE THREE DEAD MECHANISMS.  DO NOT RE-DERIVE THEM.
;;;; ============================================================
;;;;
;;;; 1. "MAKE-THREAD never arms the gate" — FALSE.  Probe A: the worker reads
;;;;    gate=1 mode=1.
;;;;
;;;; 2. "%SB-THREADS-UP clears the mode word (1 in, 0 out)" — an artifact of a
;;;;    probe file that spawned a thread first and armed everything after it.
;;;;    Probe D reproduces nothing; %HA-PERCPU-INIT-CPU is exonerated by E.
;;;;
;;;; 3. "%SB-THREADS-UP latches success WITHOUT arming, in some script shapes"
;;;;    — ALSO FALSE, and this was the one that survived longest.  It arms.
;;;;    Every "gate=0 mode=0 after a successful bringup" reading in this
;;;;    campaign — this file's old probe F, test/hosted-bringup-bare.lisp, and
;;;;    test/glass-tx-cell.lisp's per-grade-point `[gate=0/mode=0]' on the
;;;;    worker — came from ONE defect in the INSTRUMENT:
;;;;
;;;;      an inline (MEM-REF <literal address> :U32), in a TOPLEVEL FORM that
;;;;      also contains a call to a function defined by
;;;;      net/sb-thread-shim.lisp, reads the value that address held BEFORE the
;;;;      form began.
;;;;
;;;;    It is not about arming and not about %SB-THREADS-UP.  Probe F below is
;;;;    a form whose only shim call is `(sb-thread:threadp 5)' — which arms
;;;;    nothing and does nothing — on an ALREADY-armed system, and its inline
;;;;    reads still answer 0 in the same argument list in which the compiled
;;;;    accessors answer 1.  That is why B and the old F "disagreed about the
;;;;    same call": they differed in whether the READ sat in the same form as a
;;;;    shim CALL, not in what the call did.  There was never a shape that
;;;;    failed to arm, so there was never a shape question to answer.
;;;;
;;;; ***GRADE THIS SEAM WITH %RT-THREADS-LIVE-P AND %HA-PERCPU-MODE*** — ordinary
;;;; compiled functions in the image, honest everywhere — or better, with the
;;;; FUNCTIONAL oracle: %RT-THREADS-ON zeroes the acquisition counter at
;;;; #x10000DE0 and every locked section increments it, so 200 SYMBOL-VALUEs
;;;; move it by thousands after bringup and not at all before.  That observes
;;;; the lock ENGAGING without reading the gate at all.  It is what
;;;; test/hosted-bringup-bare.lisp now asserts (16 checks), and it is the
;;;; measurement that killed mechanism 3.
;;;;
;;;; ============================================================
;;;; THE PROBES
;;;; ============================================================
;;;;
;;;; A  a bare MAKE-THREAD arms the seam: worker reads gate=1 mode=1.
;;;; B  %SB-THREADS-UP alone: pre 0/0/NIL -> post ret=T gate=1 mode=1 flag=T.
;;;; C  the four steps of the body, at toplevel.  (Its gate column is read in
;;;;    the same FORMAT that invokes %RT-THREADS-ON — see probe F for why that
;;;;    is not a fact about the gate.)
;;;; D  mode set to 1 BY HAND, then the call: mode survives.  Mechanism 2 dead.
;;;; E  %HA-PERCPU-INIT-CPU with mode pre-set: mode survives.  Not a clearer.
;;;; F  ***THE ARTIFACT ITSELF***, on an already-armed system: one argument
;;;;    list, compiled-accessor reads against inline MEM-REF reads, plus the
;;;;    same inline reads from a form with no shim call in it for contrast.
;;;; G  ***THE NEGATIVE CONTROL FOR THE BRINGUP GUARD.***  %RT-THREADS-ON is
;;;;    redefined to a no-op, so the seam CANNOT arm, and %SB-THREADS-UP is
;;;;    then required to SIGNAL and to leave *SB-THREADS-UP* NIL.  On a binary
;;;;    built before that guard it returns T and latches instead — measured
;;;;    both ways, which is what makes G worth having.
;;;;
;;;; ============================================================
;;;; WHAT IS STILL NOT SHOWN
;;;; ============================================================
;;;;
;;;;   The tx-cell overwrite is NOT explained by an inert lock, because the
;;;;   lock was never inert.  Whether arming changes the overwrite is a
;;;;   question this file cannot answer; test/run-glass-tx-cell.sh can, and its
;;;;   grade points now read the seam honestly.
;;;;
;;;;   The underlying evaluator defect — why an inline MEM-REF of a literal
;;;;   address goes stale in a form containing a shim call — is CHARACTERISED
;;;;   here but NOT fixed and NOT root-caused.  It is a reason to distrust that
;;;;   idiom everywhere, not just here.
;;;;
;;;; NOTHING HERE LISTENS, CONNECTS, OR LEAVES A THREAD RUNNING: probe A joins
;;;; its one thread; the rest is memory reads.

;;; READ THE SEAM THROUGH THESE, NOT INLINE.  Both bodies are the same inline
;;; MEM-REF the probes must not write in place — inside an ordinary compiled
;;; DEFUN the read is honest, which is exactly the contrast probe F prints.
(defun bg-gate () (mem-ref #x10000DB8 :u32))
(defun bg-mode () (mem-ref #x10000FF8 :u32))

;;; PROBE G's NEUTERING MUST HAPPEN AT TOPLEVEL, BEFORE THE PROBE FORM, and it
;;; must not be evaluated for any other probe — redefining %RT-THREADS-ON is
;;; precisely the thing that makes bringup fail, so it is gated hard.
(when (let ((w (%cli-getenv "MODUS_BRINGUP_PROBE"))) (and w (string= w "G")))
  (eval '(defun %rt-threads-on () 0)))

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

    ;; ---- F: THE READ ARTIFACT ITSELF --------------------------------------
    ;; Arm the seam FIRST, in its own toplevel form, so nothing below is about
    ;; arming.  Then read the two words twice: through the compiled accessors
    ;; and as inline MEM-REFs, in ONE argument list, from a form whose only
    ;; shim call is THREADP — which arms nothing and does nothing.
    ((and which (string= which "F"))
     ;; NOTE: the whole (LET ((which …)) (COND …)) around this arm is ONE
     ;; toplevel form and it contains shim calls, so every inline MEM-REF in it
     ;; — including these — is in the affected position.  That is the point.
     (%sb-threads-up)
     (let ((ig (mem-ref #x10000DB8 :u32))
           (im (mem-ref #x10000FF8 :u32))
           (fg (bg-gate))
           (fm (bg-mode))
           (acq (%rt-acquisitions)))
       (format t "~&F fn-gate=~s fn-mode=~s inline-gate=~s inline-mode=~s acquisitions=~s~%"
               fg fm ig im acq)
       (format t "~&F VERDICT: ~a~%"
               (if (and (= fg 1) (zerop ig))
                   "REPRODUCED — an inline MEM-REF in a shim-calling toplevel form reads STALE while the compiled accessor reads the truth"
                   "did NOT reproduce in this shape; report that, do not assume it is fixed"))))

    ;; ---- G: THE NEGATIVE CONTROL FOR THE BRINGUP GUARD --------------------
    ;; %RT-THREADS-ON is neutered, so the seam CANNOT arm.  %SB-THREADS-UP is
    ;; required to SIGNAL and to leave the flag NIL.  A binary built before the
    ;; guard returns T and latches — that is the "before" half of the control,
    ;; and it is why this probe reports which half it saw rather than just
    ;; asserting.
    ((and which (string= which "G"))
     (let ((r (handler-case (list :returned (%sb-threads-up))
                (error (e) (list :signalled (princ-to-string e))))))
       (format t "~&G outcome=~s flag=~s gate=~s mode=~s~%"
               (first r) *sb-threads-up* (bg-gate) (bg-mode))
       (format t "~&G VERDICT: ~a~%"
               (if (and (eq (first r) :signalled) (null *sb-threads-up*))
                   "GUARDED — bringup that cannot arm SIGNALS and does not latch"
                   "UNGUARDED — bringup latched a partial success (pre-guard binary)"))))

    (t (format t "~&(no probe selected)~%"))))

(finish-output)
(sys-exit 0)
