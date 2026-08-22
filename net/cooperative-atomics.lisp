;;;; cooperative-atomics.lisp — atomic read-modify-write for Modus
;;;;
;;;; STATUS: NOT WIRED INTO ANY BUILD SCRIPT.  Read "WIRING THIS UP" at the
;;;; end of this header first — reaching these operations from a portable
;;;; threading library also needs net/genera-compat.lisp, which
;;;; makes Modus advertise :GENERA.  That is a campaign-level decision, not a
;;;; local one.
;;;;
;;;; ---------------------------------------------------------------------
;;;; !!! SMP LANDMINE — READ THIS BEFORE ENABLING MULTI-CORE !!!
;;;;
;;;; These operations are atomic ONLY because Modus today runs a
;;;; COOPERATIVE, SINGLE-CORE scheduler.  They contain NO lock, NO memory
;;;; barrier, and NO compare-exchange instruction.  They are ordinary
;;;; read-modify-write sequences.
;;;;
;;;; The atomicity argument rests on exactly three facts, each CHECKED in
;;;; the tree when this was written (2026-08-08, off tip 52e8eb9).  If ANY
;;;; of them stops holding, EVERY operation in this file silently becomes a
;;;; lost-update bug and must be re-implemented with real atomic
;;;; instructions (x86 LOCK CMPXCHG / XADD, aarch64 LDAXR/STLXR, riscv
;;;; LR/SC, ppc LWARX/STWCX):
;;;;
;;;;   (1) NO YIELD POINT INSIDE THE SEQUENCE.  The ONLY place the MVM
;;;;       compiler emits the YIELD opcode is at the end of a LOOP
;;;;       iteration body — there is exactly one `(emit-ir :yield)` site in
;;;;       mvm/compiler.lisp (~line 8890), inside compile-loop.  None of
;;;;       the expansions below contains a LOOP, so no yield — and
;;;;       therefore no cooperative context switch — can land between the
;;;;       read and the write.
;;;;
;;;;   (2) NOTHING PREEMPTS A RUNNING ACTOR.  mvm/interp.lisp treats
;;;;       +op-yield+ as an explicit no-op ("preemption: no-op"), and on
;;;;       hosted Linux the bare-metal safepoint-deadline stub is NIL
;;;;       (mvm/translate-x64.lisp ~line 389), so no timer can steal the
;;;;       CPU mid-sequence.  On BARE METAL x64 the PIT deadline safepoint
;;;;       IS live — but it is only consulted AT a YIELD site, so (1) still
;;;;       covers us there too.
;;;;
;;;;   (3) NO SIGNAL HANDLER CAN TOUCH THE PLACE.  The only handlers Modus
;;;;       installs are for the SYNCHRONOUS fault signals SIGSEGV / SIGBUS /
;;;;       SIGFPE / SIGILL (TRAP #x0520, mvm/translate-x64.lisp ~line 1150,
;;;;       reached from %install-signal-handlers in mvm/cl-conditions.lisp).
;;;;       That handler is an embedded ASSEMBLY stub which deliberately runs
;;;;       NO Lisp — it either longjmps to the innermost handler-case or
;;;;       sys_exit(139).  So it cannot perform a competing read-modify-
;;;;       write.  A fault landing between our read and our write ABANDONS
;;;;       the update (the write simply never happens); it cannot produce a
;;;;       torn or interleaved value.  There is no asynchronous signal in
;;;;       the hosted runtime — no setitimer / SIGALRM / SIGIO anywhere.
;;;;
;;;; When SMP is switched on — or a preemptive timer safepoint starts being
;;;; delivered to hosted code outside a YIELD site, or a Lisp-level
;;;; asynchronous signal handler is added — grep for
;;;; COOPERATIVE-ATOMIC-PRECONDITION and fix every hit.  Do not reason from
;;;; "it worked before"; the guarantee is structural, and the structure is
;;;; what changes.
;;;;
;;;; ---- WHAT PER-REGION GC STAGE 3 CHANGED HERE (2026-08-22) ----
;;;;
;;;; Stage 3 of per-region GC (mvm/gc.lisp; CLAUDE.md "Per-region GC, stage 3")
;;;; is the step whose stated purpose is to make PER-ACTOR SMP reachable, so it
;;;; is exactly the kind of change that could invalidate the three facts above
;;;; silently.  Recorded here rather than left to be rediscovered:
;;;;
;;;; ALL THREE FACTS STILL HOLD, AND FOR THE SAME REASONS.  Stage 3 added no
;;;; yield site (the ONE `(emit-ir :yield)' in mvm/compiler.lisp's compile-loop
;;;; is still the only one — fact 1); it made no scheduler preemptive
;;;; (mvm/interp.lisp still treats +op-yield+ as a no-op and the hosted
;;;; safepoint-deadline stub is still NIL — fact 2); and it installed no signal
;;;; handler (fact 3).  NO IMAGE RUNS NATIVE THREADS: every shipping build is
;;;; still single-core, and net/actors.lisp is still a COOPERATIVE scheduler
;;;; whose only context switch is an explicit YIELD / RECEIVE call.
;;;;
;;;; SO THESE OPERATIONS DID NOT BECOME UNSAFE.  They also did not become safe:
;;;; nothing here acquired a lock, a barrier or a compare-exchange.  What
;;;; changed is only how far away the falsification now is.
;;;;
;;;; WHAT STAGE 3 ACTUALLY BUILT, stated so the next reader can tell what is
;;;; load-bearing:
;;;;   - An actor names its own GC region in its struct at +0x68, and
;;;;     net/actors.lisp's YIELD and RECEIVE call %GC-REGION-SWITCH with the SP
;;;;     save-context just recorded.  That is still a COOPERATIVE switch on ONE
;;;;     CPU; it changes which heap the mutator allocates from, not when it can
;;;;     be interrupted.
;;;;   - The active-region word can now be PER-CPU (mvm/gc.lisp
;;;;     %GC-REGION-CELL, translate-x64 *X64-GC-REGION-PERCPU*).  BOTH GATES
;;;;     DEFAULT OFF and nothing writes the mode word, so no image built so far
;;;;     has more than one active-region cell.
;;;;
;;;; THE PRECONDITION THAT MUST STILL BE TRUE, restated for the SMP step this
;;;; is aimed at: the moment a SECOND CPU runs Modus code — which is what
;;;; turning the per-CPU active-region cell on is FOR — fact 1 stops being
;;;; sufficient on its own.  "No yield point inside the sequence" bounds only
;;;; THIS CPU; another core executing the same read-modify-write concurrently
;;;; needs no yield point to lose an update.  Facts 2 and 3 are equally
;;;; single-core arguments.  So: PER-ACTOR SMP AND THESE MACROS CANNOT SHIP
;;;; TOGETHER UNCHANGED.  Whoever enables the second CPU owns replacing every
;;;; COOPERATIVE-ATOMIC-PRECONDITION site below with a real atomic (x86 LOCK
;;;; CMPXCHG / XADD, aarch64 LDAXR/STLXR, riscv LR/SC, ppc LWARX/STWCX) — the
;;;; MVM already has :xchg-mem, which net/actors.lisp's SPIN-LOCK uses, so the
;;;; ISA is not the obstacle.
;;;; ---------------------------------------------------------------------
;;;;
;;;; SEMANTICS
;;;;   %ATOMIC-CAS place old new  → T if the swap happened, NIL otherwise.
;;;;   %ATOMIC-INCF place [delta] → the NEW value.
;;;;   %ATOMIC-DECF place [delta] → the NEW value.
;;;; PLACE is expanded twice (once to read, once to write), so its subforms
;;;; are evaluated twice.  OLD / NEW / DELTA are each evaluated once.
;;;;
;;;; ---------------------------------------------------------------------
;;;; WIRING THIS UP  (investigated + measured 2026-08-08; re-pointed at
;;;; :GENERA 2026-08-09 for task #237)
;;;;
;;;; The consumer these were written for is bordeaux-threads v0.9.4
;;;; apiv2/atomics.lisp, whose three OPERATION-NOT-IMPLEMENTED errors on the
;;;; library ladder are NOT a missing-operation gap.  They are a
;;;; reader-conditional wall:
;;;;
;;;;     #-(or allegro ccl clasp ecl genera lispworks sbcl)
;;;;     (signal-not-implemented 'atomic-cas)   ; likewise -incf / -decf
;;;;
;;;; bordeaux offers atomics on seven named implementations, so reaching a
;;;; Modus implementation at all requires advertising one of those seven.
;;;;
;;;; THE ANSWER IS :GENERA — see net/genera-compat.lisp for the full
;;;; argument.  In short: of every implementation the portable corpus knows
;;;; how to conditionalise for, only Genera and Mezzano were ever operating
;;;; systems; everything else's implementation-specific surface is
;;;; ultimately `ffi:', a door out to C that Modus does not have.  Mezzano
;;;; is NOT in bordeaux's atomics list; Genera is.  (An earlier revision of
;;;; this file recommended :CLASP on smallest-blast-radius grounds and
;;;; measured the ladder at 28 -> 24 errors with it.  :CLASP was rejected
;;;; because it drags in ECL's family internals and because "smallest blast
;;;; radius" optimises for not-being-recognised, which is the opposite of
;;;; the goal.)
;;;;
;;;; The Genera SURFACE (SCL / SYS / SI / PROCESS / CLI / GRAY-STREAMS /
;;;; FUTURE-COMMON-LISP, plus the feature push itself) lives in
;;;; net/genera-compat.lisp, which uses the macros below.  Genera spells
;;;; the operations PROCESS:ATOMIC-INCF / PROCESS:ATOMIC-DECF and
;;;; SYS:STORE-CONDITIONAL over a SCL:LOCF locative.
;;;;
;;;; PREREQUISITE ALREADY LANDED: this could not work at all until the
;;;; runtime macro registry stopped being package-blind.  %MACRO-SYM-KEY
;;;; keyed *MACRO-FUNCTION-TABLE* by bare name, so PROCESS:ATOMIC-INCF and
;;;; BORDEAUX-THREADS-2:ATOMIC-INCF shared one entry, and bordeaux's macro
;;;; expanding into the backend's macro resolved back to itself —
;;;; unbounded macroexpansion, a hard HANG.  Fixed by the per-package
;;;; expander table in mvm/cl-eval.lisp (%MACRO-PKG-GET / -PUT / -REM),
;;;; merged as e4d26a8.
;;;; ---------------------------------------------------------------------

;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
(defmacro %atomic-cas (place old new)
  (let ((o (gensym "OLD")) (n (gensym "NEW")))
    `(let ((,o ,old) (,n ,new))
       (if (eql ,place ,o)
           (progn (setf ,place ,n) t)
           nil))))

;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
(defmacro %atomic-incf (place &optional (delta 1))
  (let ((d (gensym "D")))
    `(let ((,d ,delta)) (setf ,place (+ ,place ,d)))))

;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
(defmacro %atomic-decf (place &optional (delta 1))
  (let ((d (gensym "D")))
    `(let ((,d ,delta)) (setf ,place (- ,place ,d)))))

;;; ---------------------------------------------------------------------
;;; The BACKEND half (the SCL / SYS / SI / PROCESS / CLI / GRAY-STREAMS /
;;; FUTURE-COMMON-LISP packages plus the :GENERA feature advertisement)
;;; lives in net/genera-compat.lisp.  Both files are runtime preludes
;;; rather than build sources: they name packages that exist only inside a
;;; running Modus, and CHECK-PARSES reads every first-party build source
;;; with SBCL's reader, which rejects `scl::locf' with "Package SCL does
;;; not exist".  Load them with
;;;   modus --load net/cooperative-atomics.lisp \
;;;         --load net/genera-compat.lisp --load <your-app>
;;; or bake them as embedded source STRINGs the way mvm/repl-source.lisp
;;; does.
;;; ---------------------------------------------------------------------
