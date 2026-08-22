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
;;;; =====================================================================
;;;; STATUS AS OF NATIVE THREADS (2026-08-22): **UNUSABLE UNDER SMP.**
;;;; THIS FILE IS NO LONGER SAFE IN A PROCESS THAT HAS STARTED A THREAD.
;;;; =====================================================================
;;;;
;;;; The falsification the three facts below were written to wait for HAS
;;;; HAPPENED.  Hosted x86-64 now runs REAL OS THREADS: clone(2) with
;;;; CLONE_VM|CLONE_THREAD and an mmap'd stack (translate-x64 TRAP #x0540,
;;;; net/hosted-actors-post.lisp %HA-SPAWN-T2), a REAL scheduler spinlock on
;;;; +OP-ATOMIC-XCHG+ released by +OP-RESTORE-CTX+, a per-thread GS base, a
;;;; per-thread active GC region, and the actor system driven from two threads
;;;; at once with messages crossing between them (test/hosted-threads.lisp,
;;;; test/hosted-thread-regions.lisp, test/hosted-thread-actors.lisp,
;;;; test/hosted-thread-gc.lisp).
;;;;
;;;; EXACTLY WHAT BROKE, fact by fact — and note that NONE of the three was
;;;; wrong; all three were arguments about ONE CPU, and there are now two:
;;;;
;;;;   (1) "no yield point inside the sequence" still holds — compile-loop is
;;;;       still the only (emit-ir :yield) site and none of the expansions below
;;;;       contains a LOOP — and it is now INSUFFICIENT.  It bounded what THIS
;;;;       CPU could do between the read and the write.  A second CPU executing
;;;;       the same read-modify-write needs no yield point to lose an update: it
;;;;       simply reads the same old value and writes over the other's result.
;;;;
;;;;   (2) "nothing preempts a running actor" still holds and is now equally
;;;;       insufficient, for the same reason: a second thread does not have to
;;;;       preempt anything to run at the same time.
;;;;
;;;;   (3) "no signal handler can touch the place" still holds — the only
;;;;       handlers are the synchronous fault ones, and CLONE_SIGHAND shares
;;;;       them rather than adding any — and is likewise a single-CPU argument.
;;;;
;;;; SO: %ATOMIC-CAS, %ATOMIC-INCF and %ATOMIC-DECF below are PLAIN
;;;; READ-MODIFY-WRITE SEQUENCES WITH NO ATOMICITY AT ALL once a second thread
;;;; exists.  They are not merely "not proven safe"; they are known-racy.  This
;;;; file is still wired into no build script, and it MUST NOT BE WIRED INTO ONE
;;;; that starts a thread.
;;;;
;;;; WHY THEY WERE NOT SIMPLY REWRITTEN ON +OP-ATOMIC-XCHG+.  The MVM has
;;;; exactly one atomic primitive, XCHG-MEM — an UNCONDITIONAL exchange at a raw
;;;; ADDRESS.  Neither half of that fits:
;;;;   - an unconditional exchange is not a compare-and-swap and cannot be made
;;;;     into one without a loop, and a CAS loop needs a CAS;
;;;;   - PLACE here is an arbitrary CL place (a cons car, a struct slot, a
;;;;     special), not an address, so there is nothing to hand XCHG-MEM anyway.
;;;;
;;;; THE TWO REAL FIXES, so the next reader does not have to re-derive them:
;;;;   (a) ADD A CAS OPCODE.  x86 LOCK CMPXCHG, aarch64 LDAXR/STLXR, riscv
;;;;       LR/SC, ppc LWARX/STWCX — the ISAs all have it; the MVM does not.
;;;;       That plus an address-of for the place is the lock-free answer.
;;;;   (b) TAKE A LOCK.  net/actors.lisp's SPIN-LOCK on XCHG-MEM is real now, so
;;;;       wrapping each sequence in acquire/release at one dedicated word is a
;;;;       CORRECT (coarse) implementation today — provided EVERY mutator of
;;;;       those places goes through these macros, which is a property of the
;;;;       consumer, not of this file.  Note the word must not be the SCHEDULER
;;;;       lock: +OP-RESTORE-CTX+ zeroes that one on every context switch.
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
;;;; TOGETHER UNCHANGED.
;;;;
;;;; **THAT MOMENT ARRIVED.**  The per-CPU active-region cell is ON in the
;;;; hosted x86-64 CLI and a second OS THREAD runs Modus code beside the first.
;;;; See the UNUSABLE UNDER SMP block at the top of this header, which is the
;;;; current status; everything from here down is the record of how it got
;;;; there.  The remark that "the MVM already has :xchg-mem, so the ISA is not
;;;; the obstacle" was too optimistic and is corrected up there: XCHG-MEM is an
;;;; UNCONDITIONAL exchange at a raw ADDRESS, which is neither a CAS nor
;;;; applicable to an arbitrary CL place.
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

;;; COOPERATIVE-ATOMIC-PRECONDITION: **FALSIFIED — SMP.**  No LOOP still means
;;; no YIELD, which still means no interleaving BY THIS CPU.  It has meant
;;; nothing about a second thread since native threads landed, and there are now
;;; two on hosted x86-64.  This expansion is a plain read-modify-write and it
;;; loses updates.  See the UNUSABLE UNDER SMP block in the header.
(defmacro %atomic-cas (place old new)
  (let ((o (gensym "OLD")) (n (gensym "NEW")))
    `(let ((,o ,old) (,n ,new))
       (if (eql ,place ,o)
           (progn (setf ,place ,n) t)
           nil))))

;;; COOPERATIVE-ATOMIC-PRECONDITION: **FALSIFIED — SMP.**  No LOOP still means
;;; no YIELD, which still means no interleaving BY THIS CPU.  It has meant
;;; nothing about a second thread since native threads landed, and there are now
;;; two on hosted x86-64.  This expansion is a plain read-modify-write and it
;;; loses updates.  See the UNUSABLE UNDER SMP block in the header.
(defmacro %atomic-incf (place &optional (delta 1))
  (let ((d (gensym "D")))
    `(let ((,d ,delta)) (setf ,place (+ ,place ,d)))))

;;; COOPERATIVE-ATOMIC-PRECONDITION: **FALSIFIED — SMP.**  No LOOP still means
;;; no YIELD, which still means no interleaving BY THIS CPU.  It has meant
;;; nothing about a second thread since native threads landed, and there are now
;;; two on hosted x86-64.  This expansion is a plain read-modify-write and it
;;; loses updates.  See the UNUSABLE UNDER SMP block in the header.
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
