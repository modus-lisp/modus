;;;; hosted-thread-regions.lisp — STEP 3: A PER-THREAD ACTIVE GC REGION.
;;;;
;;;;   ./modus --script test/hosted-thread-regions.lisp
;;;;
;;;; Per-region GC stage 3 built this and left it OFF.  The active-region word
;;;; became CPU 0's entry of a 16-entry array based at the SAME address
;;;; (+GC-REGION-ADDR+ + 8*cpu_id), gated on both sides — the Lisp side
;;;; (mvm/gc.lisp %GC-REGION-CELL) on the BSS mode word 0x10000FF8, the native
;;;; side (translate-x64 EMIT-LOAD-GC-REGION) on *X64-GC-REGION-PERCPU*.
;;;; Nothing ever wrote the mode word, so the per-CPU form had never executed in
;;;; any image.  This turns it on, with a second OS thread to turn it on FOR.
;;;;
;;;; THE NATIVE FLAG IS :RUNTIME, NOT T, AND THAT IS THE DESIGN.  ./modus is one
;;;; binary serving two populations: ordinary single-threaded runs, where the GS
;;;; base is 0 and an unguarded `GS:[16]' takes SIGSEGV on the first collection,
;;;; and threaded runs, where every thread has a per-CPU block and the collector
;;;; must read THIS thread's cell.  A build-time T would make the first
;;;; population crash.  So EMIT-LOAD-GC-REGION emits BOTH forms and branches on
;;;; the SAME mode word %GC-REGION-CELL branches on: one word flips the mutator
;;;; and the collector together, and they cannot read different cells.  The
;;;; branch is paid three times per COLLECTION (all three callers are inside the
;;;; GC trampoline), never in an allocation fast path.
;;;;
;;;; ORDERING IS THE SAFETY ARGUMENT for turning it on at runtime:
;;;;   1. the main thread points GS at a real per-CPU block, stamps CPU 0;
;;;;   2. ONLY THEN is the mode word set;
;;;;   3. clone is issued WITHOUT CLONE_SETTLS (that sets FS on x86-64, and this
;;;;      is GS), so the new thread INHERITS the parent's GS base — the wrong
;;;;      cell for a moment, but never an unmapped one — and it reads no region
;;;;      until it has installed its own block and stamped CPU 1.
;;;;
;;;; WHAT MAKES THE ISOLATION CLAIM A MEASUREMENT AND NOT A TAUTOLOGY:
;;;;
;;;;   - The sharpest single number is what thread 2's cell says BEFORE thread 2
;;;;     touches it.  Thread 1 has already put ITS region in ITS cell; thread
;;;;     2's cell is still the BSS zero, so it must answer REGION 0.  If the
;;;;     per-CPU indexing were not working, thread 2 would be reading thread 1's
;;;;     cell and would see thread 1's region there.
;;;;   - "My cell never changed while the other thread switched" is worthless if
;;;;     the other thread never actually switched during the window.  So the
;;;;     watcher ALSO samples the switcher's RAW cell address and counts how
;;;;     many DISTINCT values it saw; that count is required to be >= 2.
;;;;   - It runs in BOTH directions: phase A has thread 1 switching and thread 2
;;;;     watching, phase B swaps them, with their own barrier.
;;;;   - The two cell addresses are read back and required to be the two
;;;;     DIFFERENT table entries, eight bytes apart.
;;;;
;;;; AND R12/R14 ARE PER-THREAD TOO, which is the other half of "its own
;;;; region".  Thread 2 adopts its region through the load half of
;;;; %GC-REGION-ENTER and comes out with an allocation pointer inside ITS OWN
;;;; semispace, while thread 1's allocation pointer is unchanged and still in
;;;; region 0.  Thread 2 does NOT use %GC-REGION-ENTER itself: ENTER parks the
;;;; LEAVING region's R12/R14, and a fresh thread's R12/R14 are a stale COPY of
;;;; the spawning thread's — parking them into region 0's shared block would
;;;; roll another thread's allocation frontier backwards.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *budget* 200000000)
(defvar *work* 2000000)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(let ((res (%ha-regions-percpu-selftest *budget* *work*)))
  (if (= res 0)
      (format t "~%SKIP: no actor band, or the thread stack could not be mapped.~%")
      (let* ((rc1     (w res #x00))
             (mode0   (w res #x08))
             (mode1   (w res #x10))
             (cpu1    (w res #x18))
             (cell1   (w res #x20))
             (t1reg   (w res #x28))
             (tid     (w res #x30))
             (join    (w res #x38))
             (rc2     (w res #x40))
             (cpu2    (w res #x48))
             (cell2   (w res #x50))
             (t2pre   (w res #x58))
             (t2reg   (w res #x60))
             (rcb2    (w res #x68))
             (rcb3    (w res #x70))
             (r0      (w res #x78))
             (started (w res #x80))
             (t2toA   (w res #x88))
             (t1toA   (w res #x90))
             (t2toB   (w res #x98))
             (t1toB   (w res #xA0))
             (t2bad   (w res #xA8))
             (t2seen  (w res #xB0))
             (t1bad   (w res #xB8))
             (t1seen  (w res #xC0))
             (t1sw    (w res #xC8))
             (t2sw    (w res #xD0))
             (work    (w res #xD8))
             (t2alloc (w res #xE0))
             (t2limit (w res #xE8))
             (t1a0    (w res #xF0))
             (t1a1    (w res #xF8))
             (clean   (w res #x100))
             (cell2b  (w res #x108))
             (r2from  (w res #x110))
             (t1end   (w res #x118))
             (t1inr   (w res #x120))
             (r1from  (w res #x128))
             (g0      (w res #x130))
             (g2      (w res #x138))
             (g3      (w res #x140)))

        (format t "~%=== THE GATE WORD ========================================~%")
        (chk "the per-CPU mode word before this test" mode0 0)
        (chk "arch_prctl(ARCH_SET_GS) for thread 1" rc1 0)
        (chk "arch_prctl(ARCH_SET_GS) for thread 2" rc2 0)
        (chk "the mode word after turning it on" mode1 1)
        (format t "  (turned on AFTER a real per-CPU block was installed, never~%")
        (format t "   before: with mode=1 and GS=0 the cell read is address 16.)~%")

        (format t "~%=== TWO CPUs, TWO CELLS ==================================~%")
        (format t "  thread 1 cpu-id ~D  cell ~X~%" cpu1 cell1)
        (format t "  thread 2 cpu-id ~D  cell ~X~%" cpu2 cell2)
        (chk "thread 1 reads CPU id" cpu1 0)
        (chk "thread 2 reads CPU id" cpu2 1)
        (chk "thread 1's cell is +GC-REGION-ADDR+ (the historic word)"
             cell1 #x10000F08)
        (chk "thread 2's cell is the next table entry" cell2 (+ cell1 8))
        (chk "and the driver computed the same address for CPU 1's cell"
             cell2b cell2)

        (format t "~%=== EACH THREAD READS ITS OWN CELL =======================~%")
        (format t "  region 0's block ~X ; thread 1's region ~X ; thread 2's ~X~%"
                r0 rcb2 rcb3)
        (chk "thread 1's active region after it set its own cell" t1reg rcb2)
        (format t "  Thread 2 looked at ITS cell BEFORE touching it, at a moment~%")
        (format t "  when thread 1's cell already held ~X.  A shared cell would~%" rcb2)
        (format t "  have shown thread 1's region here.~%")
        (chk "what thread 2 saw in its own untouched cell" t2pre r0)
        (chk "thread 2's active region after it adopted its own" t2reg rcb3)
        (chk "the second thread ran" started 1)
        (chk-true "clone returned a TID" (> tid 0))

        (format t "~%=== SWITCHING ONE CELL DOES NOT MOVE THE OTHER ===========~%")
        (format t "  PHASE A — thread 1 switches its cell ~D times between~%" work)
        (format t "  region 0 and its own; thread 2 samples ITS cell ~D times.~%" work)
        (chk "phase A: thread 1 reached the barrier alone (1 = yes)" t1toA 0)
        (chk "phase A: thread 2 reached the barrier alone (1 = yes)" t2toA 0)
        (chk "thread 1 switches performed" t1sw work)
        (chk "samples where thread 2's own cell was NOT its region" t2bad 0)
        (format t "  POSITIVE CONTROL: distinct values thread 2 saw in thread~%")
        (format t "  1's raw cell during that window = ~D.  If this were 1 the~%" t2seen)
        (format t "  zero above would only mean nothing was happening.~%")
        (chk-true "thread 2 observed thread 1's cell actually changing"
                  (>= t2seen 2))

        (format t "~%  PHASE B — the same experiment with the roles swapped.~%")
        (chk "phase B: thread 1 reached the barrier alone (1 = yes)" t1toB 0)
        (chk "phase B: thread 2 reached the barrier alone (1 = yes)" t2toB 0)
        (chk "thread 2 switches performed" t2sw work)
        (chk "samples where thread 1's own cell was NOT its region" t1bad 0)
        (format t "  POSITIVE CONTROL: distinct values thread 1 saw in thread~%")
        (format t "  2's raw cell = ~D~%" t1seen)
        (chk-true "thread 1 observed thread 2's cell actually changing"
                  (>= t1seen 2))

        (format t "~%=== AND R12/R14 ARE PER-THREAD TOO =======================~%")
        (format t "  thread 2 alloc ptr / limit after adopting its region:~%")
        (format t "    ~X .. ~X~%" t2alloc t2limit)
        (format t "  thread 2's own from-space starts at ~X~%" r2from)
        (chk "thread 2's allocation pointer is its region's from-space start"
             t2alloc r2from)
        (chk-true "and its limit is a whole semispace above that"
                  (> t2limit t2alloc))
        (format t "  thread 1 alloc ptr inside its own region ~X~%" t1inr)
        (format t "  thread 1's own from-space starts at      ~X~%" r1from)
        (chk "thread 1's allocation pointer is ITS region's from-space start"
             t1inr r1from)
        (chk-true "the two threads' allocation pointers are DIFFERENT"
                  (not (= t1inr t2alloc)))
        (format t "  thread 1's region-0 alloc ptr before ~X, after ~X~%" t1a0 t1a1)
        (chk "and %GC-REGION-ENTER put it back EXACTLY on the way out"
             (- t1a1 t1a0) 0)

        (format t "~%=== NOBODY COLLECTED, MEASURED NOT ASSUMED ===============~%")
        (format t "  The switch loop leaves a cell naming region 0 for part of~%")
        (format t "  every iteration while R12/R14 belong to the switcher's own~%")
        (format t "  region, so a collection inside that window would evacuate~%")
        (format t "  the wrong heap.  Neither loop allocates, by construction —~%")
        (format t "  and these three counts are the evidence.~%")
        (chk "region 0 collections during the run" g0 0)
        (chk "thread 1's region collections" g2 0)
        (chk "thread 2's region collections" g3 0)

        (format t "~%=== A CLEAN EXIT, AND THE MODE PUT BACK ==================~%")
        (chk "thread 2 reached the end of its entry function" clean 1)
        (chk "the join saw the kernel clear the TID word (0 = joined)" join 0)
        (chk "the driver is back in region 0" t1end r0)
        (chk "and the mode word is back where it was" (%ha-percpu-mode) mode0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "PER-THREAD ACTIVE REGION: PASS (~D checks)~%" *checks*)
            (format t "PER-THREAD ACTIVE REGION: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
