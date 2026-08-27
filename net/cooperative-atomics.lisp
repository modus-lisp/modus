;;;; cooperative-atomics.lisp — atomic read-modify-write for Modus
;;;;
;;;; ======================================================================
;;;; STATUS (2026-08-27): **TWO OF THREE ARE FIXED.  %ATOMIC-CAS IS NOT.**
;;;;
;;;;   %ATOMIC-INCF   SMP-safe — spinlock on +OP-ATOMIC-XCHG+, gated on
;;;;   %ATOMIC-DECF   %RT-THREADS-LIVE-P.  See THE IMPLEMENTATION below.
;;;;
;;;;   %ATOMIC-CAS    **STILL A PLAIN READ-MODIFY-WRITE.  STILL LOSES
;;;;                  UPDATES UNDER SMP.  DO NOT USE IT FROM MORE THAN ONE
;;;;                  THREAD.**  It is blocked on an INSTRUCTION, not on a
;;;;                  design: XCHG is an UNCONDITIONAL exchange and cannot
;;;;                  express compare-and-swap, and the MVM has no
;;;;                  compare-exchange opcode on any back-end.  It could have
;;;;                  been wrapped in the same spinlock — the read, the
;;;;                  compare and the write would then be one critical
;;;;                  section and it would in fact be correct — but ONLY for
;;;;                  callers that route every mutation of that place through
;;;;                  these macros, and a CAS's whole reason for existing is
;;;;                  to be safe against mutators that do NOT.  A lock-based
;;;;                  CAS would test clean against a lock-based INCF and be a
;;;;                  lie to the first caller that mixed it with a plain
;;;;                  SETF.  It keeps its FALSIFIED marker until LOCK CMPXCHG
;;;;                  (x64) and LDAXR/STLXR (aarch64) exist.
;;;; ======================================================================
;;;;
;;;; HOW THE FILE REACHES A RUNNING IMAGE — it is WIRED IN AND LIVE, and that
;;;; is what made the falsification below a hazard rather than a note.  This
;;;; line used to read "NOT WIRED INTO ANY BUILD SCRIPT"; that stopped being
;;;; true and nobody updated it.  Measured in the shipping `./modus`
;;;; (2026-08-27):
;;;;
;;;;   :GENERA in *FEATURES*        => T      (so bordeaux-threads takes its
;;;;                                           Genera branch, by design)
;;;;   PROCESS:ATOMIC-INCF          => bound
;;;;   SYS:STORE-CONDITIONAL        => bound
;;;;
;;;; It reaches the image as *GENERA-COMPAT-TEXT* (mvm/build-cli-common.lisp
;;;; ~1301) and is installed by %INSTALL-GENERA-COMPAT (~1230).
;;;;
;;;; So a portable threading library loading on Modus today gets THESE
;;;; operations — and see the UNUSABLE UNDER SMP block below: Modus now has
;;;; native OS threads, which falsifies the precondition every one of them
;;;; rests on.  The wiring was correct when the runtime was cooperative; the
;;;; runtime changed underneath it.
;;;;
;;;; THE FIX IS NOW AVAILABLE AND WAS NOT BEFORE.  Modus has a real mutex
;;;; (%MUTEX-LOCK / %MUTEX-UNLOCK, net/hosted-sync.lisp, acceptance
;;;; test/hosted-mutex.lisp 18 checks) and a real XCHG (+OP-ATOMIC-XCHG+).
;;;; Either route works: take a single global atomics mutex when the threads
;;;; gate is live (%RT-THREADS-LIVE-P) and fall through to today's code when it
;;;; is not — the same default-off shape used everywhere else in this tree —
;;;; or add LOCK CMPXCHG to the translator, which %ATOMIC-CAS needs since XCHG
;;;; is an unconditional exchange and cannot express compare-and-swap.
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
;;;; ---------------------------------------------------------------------
;;;; THE REPLACEMENT DESIGN (2026-08-27) — and why it is NOT "hosted only"
;;;; ---------------------------------------------------------------------
;;;;
;;;; DO NOT justify the bare-metal arm with "the scheduler there is
;;;; cooperative."  That is true today and it is the wrong thing to build on.
;;;; Measured on this tree:
;;;;
;;;;   * PSCI CPU_ON is GENERATED but never CALLED (boot/boot-aarch64.lisp
;;;;     ~678, +PSCI-CPU-ON-64+ #xC4000003).  RISC-V SBI hart-start and
;;;;     PPC64 OPAL thread-start generators exist in the same state.
;;;;   * WAKE-IDLE-AP on bare metal is `(defun wake-idle-ap () 0)`
;;;;     (net/actors.lisp ~301) — a no-op.  Only net/hosted-sync.lisp ~649
;;;;     has a real one.
;;;;
;;;; So bare metal is single-core *because nobody has called the function
;;;; yet*, not because of anything structural — and bringing up a second core
;;;; is active work.  An implementation whose correctness rests on that would
;;;; fail the day CPU_ON is called, silently, as lost updates inside the
;;;; primitives a portability library builds on.  That is precisely the
;;;; mistake this header already made once.
;;;;
;;;; WHAT IS ACTUALLY AVAILABLE, on both backends, today:
;;;;
;;;;   +OP-ATOMIC-XCHG+   translate-x64      present
;;;;   +OP-ATOMIC-XCHG+   translate-aarch64  present
;;;;   any compare-exchange                   ABSENT EVERYWHERE
;;;;
;;;; SPIN-LOCK (net/actors.lisp) is already built on XCHG-MEM and is
;;;; bare-metal code, so a real atomic exchange is proven on both
;;;; architectures.  That is enough for two of the three operations:
;;;;
;;;;   %ATOMIC-INCF / %ATOMIC-DECF — a spinlock on XCHG, on EVERY target.
;;;;       Correct without any appeal to how the scheduler behaves, and it
;;;;       stays correct when the second core comes up.  No mutex, so bare
;;;;       metal is not a special case (%MUTEX-LOCK is hosted-only —
;;;;       net/hosted-sync.lisp is inside the (and (eq *cli-arch* :x64)
;;;;       (not *cli-bare-metal*)) group in mvm/build-cli-common.lisp).
;;;;
;;;;   %ATOMIC-CAS — BLOCKED ON AN INSTRUCTION, not on a design.  XCHG is an
;;;;       UNCONDITIONAL exchange and cannot express compare-and-swap.  This
;;;;       needs LOCK CMPXCHG on x64 and LDAXR/STLXR on aarch64 added to the
;;;;       translators.  Until then a CAS built from XCHG would be a lie.
;;;;
;;;; THE GATE IS "MORE THAN ONE THREAD OF CONTROL IS LIVE", not "is this
;;;; hosted".  %RT-THREADS-LIVE-P (mvm/prelude.lisp — NOT in the hosted-only
;;;; group, so it exists on every target and reads 0 where nothing armed it)
;;;; is the hosted half.  Bare-metal SMP must arm the same predicate when it
;;;; starts a second CPU, so the atomics flip to the safe path by themselves
;;;; rather than by someone remembering.
;;;;
;;;; HOW TO TEST IT WITHOUT HARDWARE: `-smp 2` under QEMU is sufficient.  The
;;;; oracle already exists in test/hosted-mutex.lisp — its negative control
;;;; loses 261,813 of 600,000 unprotected increments and exactly 0 under the
;;;; mutex.  A bare-metal port of that shape is the acceptance test; an
;;;; atomics test that cannot lose an update when the protection is removed
;;;; is not testing anything.
;;;;
;;;; =====================================================================
;;;; STATUS AS OF NATIVE THREADS (2026-08-22): **UNUSABLE UNDER SMP.**
;;;; THIS FILE IS NO LONGER SAFE IN A PROCESS THAT HAS STARTED A THREAD.
;;;;
;;;; **SUPERSEDED FOR INCF/DECF (2026-08-27), STILL EXACTLY TRUE FOR
;;;; %ATOMIC-CAS.**  The diagnosis below is correct and is why the fix took
;;;; the shape it did; read it, then read THE IMPLEMENTATION above for what
;;;; was built.  Every sentence here that says "all three" now means CAS.
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
;;;; ---- CORRECTED ON BOTH COUNTS (2026-08-27) ----
;;;; "wired into no build script" was ALREADY FALSE when that sentence was
;;;; written — mvm/build-cli-common.lisp bakes this file as *GENERA-COMPAT-TEXT*
;;;; and %INSTALL-GENERA-COMPAT evaluates it at boot in every CLI image, which
;;;; is why the header now opens with that measurement.  And %ATOMIC-INCF /
;;;; %ATOMIC-DECF are no longer plain read-modify-writes: measured on FOUR
;;;; NATIVE OS THREADS on hosted x86-64 (a real multiprocessor, not a QEMU
;;;; -smp flag — the host has 116 cores), the armed path is EXACT and the same
;;;; macro one gate word apart loses tens of thousands of updates.  %ATOMIC-CAS is untouched
;;;; and this paragraph still describes it exactly.
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
;;;; ---------------------------------------------------------------------
;;;; THE IMPLEMENTATION (2026-08-27) — what was actually built
;;;; ---------------------------------------------------------------------
;;;;
;;;; %ATOMIC-INCF and %ATOMIC-DECF now expand to
;;;;
;;;;     (let ((d DELTA))
;;;;       (if (= (%rt-threads-live-p) 0)
;;;;           (setf PLACE (+ PLACE d))          ; <- TODAY'S BODY, VERBATIM
;;;;           (progn (%atomics-acquire)
;;;;                  (unwind-protect (setf PLACE (+ PLACE d))
;;;;                    (%atomics-release)))))
;;;;
;;;; and the four things that are load-bearing about that shape:
;;;;
;;;; 1. THE GATE IS "MORE THAN ONE THREAD OF CONTROL IS LIVE", NOT "IS THIS
;;;;    HOSTED".  %RT-THREADS-LIVE-P is in mvm/prelude.lisp, OUTSIDE the
;;;;    (and (eq *cli-arch* :x64) (not *cli-bare-metal*)) group in
;;;;    mvm/build-cli-common.lisp, so it exists on every target and reads the
;;;;    BSS zero where nothing armed it.  Bare-metal SMP must arm the SAME
;;;;    predicate when it calls PSCI CPU_ON (boot/boot-aarch64.lisp ~678) /
;;;;    SBI hart-start / OPAL thread-start, and these operations flip to the
;;;;    safe path by themselves rather than by someone remembering.
;;;;
;;;; 2. THE UNARMED ARM IS THE OLD CODE, NOT A REIMPLEMENTATION OF IT.  The
;;;;    then-branch is character-for-character the entire body of the macro
;;;;    this replaces.  An unarmed target pays exactly what %RT-ENTER,
;;;;    %DYNBIND and the B-lite arena already make every image pay: one load
;;;;    of #x10000DB8 and a branch.  It is NOT byte-identical EMISSION — a
;;;;    runtime gate cannot be — and claiming so would be the kind of
;;;;    overstatement this file exists to prevent.
;;;;
;;;; 3. A SPINLOCK ON +OP-ATOMIC-XCHG+, NOT A MUTEX, SO BARE METAL IS NOT A
;;;;    SPECIAL CASE.  %MUTEX-LOCK lives in net/hosted-sync.lisp, which IS
;;;;    inside that hosted-only group.  XCHG is present in BOTH translate-x64
;;;;    (LOCK XCHG [mem],reg — x86 locks XCHG implicitly) and
;;;;    translate-aarch64, and net/actors.lisp's SPIN-LOCK is already built on
;;;;    it and is bare-metal code, so the primitive is proven on both
;;;;    architectures.  %ATOMICS-ACQUIRE is a TTAS copy of that lock rather
;;;;    than a call to it, deliberately: SPIN-LOCK is overridden to
;;;;    `(defun spin-lock (addr) nil)' — a NO-OP — in net/arch-x86.lisp,
;;;;    net/arch-aarch64.lisp, net/arch-raspi3b.lisp, net/arch-arm32*.lisp,
;;;;    net/arch-rpi-cl.lisp and net/32bit-overrides.lisp, and which
;;;;    definition wins is a LOAD-ORDER question in each build script.  A
;;;;    lock that silently degrades to a no-op in some images is exactly the
;;;;    "unsafe state indistinguishable from a safe one" this campaign keeps
;;;;    paying for.
;;;;
;;;; 4. THE LOCK IS RELEASED ON A NON-LOCAL EXIT.  PLACE is an arbitrary CL
;;;;    place and is expanded twice; a TYPE-ERROR or a THROW out of it would
;;;;    otherwise leave the word held forever, and a hang is the one failure
;;;;    mode that tells you nothing.  Hence UNWIND-PROTECT.
;;;;
;;;; THE LOCK IS *NOT* THE SCHEDULER LOCK, and that is not a preference:
;;;; +OP-RESTORE-CTX+ stores zero to +HOSTED-SCHED-LOCK-ADDR+ (#x10000FC0) on
;;;; every context switch, so a critical section held there would be silently
;;;; released by an unrelated YIELD.  #x10000FE0 is in the BSS gap
;;;; mvm/compiler.lisp documents as free (nothing between the per-region table
;;;; at #x10000F08..F88, SMP-INIT's three scheduler-lock words #x10000FC0/FC8/
;;;; FD0, and the per-CPU mode word at #x10000FF8), and it is NOT a per-thread
;;;; window slot — %TLS-WINDOW-OFFSET-P covers offsets #x090..#x138, #x150,
;;;; #x180..#x19F, #x400..#xC0F and #xC10..#xC37 only — so every thread reads
;;;; the same word.  BSS zero-fill means "unlocked" with nothing to initialise.
;;;;
;;;; **A LANDMINE FOR WHOEVER ARMS THE GATE ON BARE METAL: ZERO THE LOCK WORD
;;;; FIRST.**  Hosted, #x10000FE0 is BSS and the kernel zero-fills it.  On bare
;;;; metal the #x1000xxxx window is ordinary RAM standing in for a BSS, and
;;;; build-rpi-cl-repl.lisp's kernel-main prologue zeroes an ENUMERATED LIST of
;;;; those words — this one is not on it, because nothing reads it today.
;;;; Uninitialised RAM that happens to hold a non-zero byte IS A PERMANENTLY
;;;; HELD LOCK, i.e. the first %ATOMIC-INCF after SMP bringup hangs forever.
;;;; This is not hypothetical: mvm/build-pizero2w-actors.lisp carries the line
;;;; "Clear TCP send lock — uninitialized RAM deadlocks spin-lock" for exactly
;;;; this reason.  %ATOMICS-LOCK-ADDR is a one-defun SEAM precisely so a target
;;;; whose free-metadata map differs can point it somewhere else; whichever
;;;; word it names must be zeroed before the gate opens.
;;;;
;;;; THE ACQUISITION COUNTER AT #x10000FE8 EXISTS SO A ZERO CAN BE TRUSTED.
;;;; It is bumped INSIDE the critical section (so a plain RMW is correct
;;;; there) and only ever by the armed path, which makes it a two-way oracle
;;;; rather than a statistic: gate off, it must not move at all across a batch
;;;; of atomics; gate on, it must move by EXACTLY the number of operations
;;;; performed.  A "the gate bypassed the lock" claim backed only by "the
;;;; answer was right" would pass equally if the lock had been taken every
;;;; time.
;;;;
;;;; NOT REENTRANT, stated because the failure is a hang: a PLACE whose own
;;;; subforms perform an %ATOMIC-INCF self-deadlocks.  Nesting these is not
;;;; supported and there is no owner check.
;;;;
;;;; THE SPIN DOES NOT CONTEXT-SWITCH.  Its LOOP emits the YIELD opcode, which
;;;; mvm/interp.lisp treats as a no-op and translate-aarch64 renders as
;;;; SEV/WFE; neither is a scheduler switch, so a cooperative actor cannot
;;;; yield the CPU while holding this lock and cannot deadlock a cooperative
;;;; peer against it.
;;;;
;;;; ACCEPTANCE: test/hosted-atomics.lisp + test/run-atomics.sh.  FOUR NATIVE
;;;; OS THREADS on hosted x86-64, and the negative control is the SAME MACRO
;;;; one gate word apart: MODUS_ATOMICS_MODE=unsync loses tens of thousands
;;;; of 80000 updates, the armed arm is exactly 80000, and a run in which the
;;;; control came out exact would FAIL the control rather than pass the test.
;;;;
;;;; ---------------------------------------------------------------------
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

;;; COOPERATIVE-ATOMIC-PRECONDITION: **FALSIFIED — SMP.  STILL FALSIFIED.  THIS
;;; ONE WAS NOT FIXED WHEN INCF AND DECF WERE.**  No LOOP still means no YIELD,
;;; which still means no interleaving BY THIS CPU.  It has meant nothing about a
;;; second thread since native threads landed, and there are now two on hosted
;;; x86-64.  This expansion is a plain read-modify-write and it loses updates.
;;;
;;; IT IS DELIBERATELY LEFT ALONE, and the reason is in the STATUS block at the
;;; top: the spinlock two operations below would make this expansion internally
;;; correct, and that correctness would only hold for callers that route EVERY
;;; mutation of PLACE through these macros — which is precisely the assumption a
;;; compare-and-swap exists to avoid needing.  A lock-based CAS passes a
;;; lock-based test suite and lies to the first caller that mixes it with SETF.
;;; The honest fix is an INSTRUCTION: LOCK CMPXCHG (x64), LDAXR/STLXR (aarch64),
;;; LR/SC (riscv), LWARX/STWCX (ppc), plus an address-of for the place.  Until
;;; then: DO NOT CALL THIS FROM MORE THAN ONE THREAD.
(defmacro %atomic-cas (place old new)
  (let ((o (gensym "OLD")) (n (gensym "NEW")))
    `(let ((,o ,old) (,n ,new))
       (if (eql ,place ,o)
           (progn (setf ,place ,n) t)
           nil))))

;;; ---------------------------------------------------------------------
;;; THE LOCK — a TTAS spinlock on +OP-ATOMIC-XCHG+, on EVERY target.
;;; ---------------------------------------------------------------------
;;;
;;; This is net/actors.lisp's SPIN-LOCK, copied rather than called: see point 3
;;; of THE IMPLEMENTATION in the header for why calling it would be a lock that
;;; silently degrades to a no-op in half the images in the tree.
;;;
;;; THE INNER WAIT READS :U8 AND NOT :U64, and that is not cosmetic — it is the
;;; bug net/actors.lisp records at length.  XCHG-MEM writes the RAW machine word
;;; 1; a `:u64' load hands a machine word back as a TAGGED Lisp value, so a held
;;; lock reads back as something `zerop' is happy to call unlocked and the
;;; acquire degenerates into an unbounded XCHG hammer (measured there at 2.0 s
;;; typical / 260 s worst for ten sends, versus 0.1 s with the read-only wait).
;;; A `:u8' load is tagged on the way out, so the value IS the raw byte.

(defun %atomics-lock-addr () #x10000FE0)

(defun %atomics-acquisitions-addr () #x10000FE8)

(defun %atomics-acquisitions ()
  "How many times the ARMED path has taken the lock.  Zero on every unarmed
   target, by construction: the gate-off branch never calls %ATOMICS-ACQUIRE."
  (mem-ref (%atomics-acquisitions-addr) :u64))

(defun %atomics-acquire ()
  (let ((a (%atomics-lock-addr)))
    (loop
      (if (zerop (xchg-mem a 1))
          (progn
            ;; Inside the critical section, so a plain read-modify-write of the
            ;; counter is correct here and nowhere else in this file.
            (setf (mem-ref (%atomics-acquisitions-addr) :u64)
                  (+ (mem-ref (%atomics-acquisitions-addr) :u64) 1))
            (return 0))
          (loop
            (if (zerop (mem-ref a :u8))
                (return 0)
                (pause)))))))

(defun %atomics-release ()
  (mfence)
  (setf (mem-ref (%atomics-lock-addr) :u64) 0)
  0)

;;; COOPERATIVE-ATOMIC-PRECONDITION: **RETIRED — this operation no longer rests
;;; on it.**  The three facts were all arguments about ONE CPU and all three
;;; remain true and insufficient; the armed path does not appeal to any of them,
;;; and the unarmed path is the pre-SMP code reached only when the process has
;;; one thread of control, where they hold.
(defmacro %atomic-incf (place &optional (delta 1))
  (let ((d (gensym "D")))
    `(let ((,d ,delta))
       (if (= (%rt-threads-live-p) 0)
           (setf ,place (+ ,place ,d))
           (progn
             (%atomics-acquire)
             (unwind-protect (setf ,place (+ ,place ,d))
               (%atomics-release)))))))

;;; COOPERATIVE-ATOMIC-PRECONDITION: **RETIRED** — see %ATOMIC-INCF above.
(defmacro %atomic-decf (place &optional (delta 1))
  (let ((d (gensym "D")))
    `(let ((,d ,delta))
       (if (= (%rt-threads-live-p) 0)
           (setf ,place (- ,place ,d))
           (progn
             (%atomics-acquire)
             (unwind-protect (setf ,place (- ,place ,d))
               (%atomics-release)))))))

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
