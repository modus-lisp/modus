;;;; bare-threads.lisp - native threads on the Pi's cores (MODUS_RPI_THREADS=1).
;;;;
;;;; Linked after net/bare-sync.lisp, whose single-threaded answers it replaces by
;;;; last-defun-wins.
;;;;
;;;; THE MODEL (docs/actors-on-coroutines.md, on bare metal).  Cores 1..3 are released
;;;; from the firmware's spin table once each, into a SCHEDULER (%BT-CORE-MAIN).  A
;;;; THREAD is a SLOT: a stack, a per-thread window and a per-CPU block of its own,
;;;; pinned to one core and run there as a coroutine.  The slots of a core SHARE that
;;;; core's heap (one GC region per core); a slot runs until it blocks (a mutex, a
;;;; condition wait, a sleep, BT-YIELD) or ends, and the core then runs another.  So
;;;; there can be more threads than cores, and a thread that waits costs no core.
;;;;
;;;; THE CONTRACT IS HOSTED AARCH64'S, so translate-aarch64's collector works as is.
;;;; The thread table is the thread page (0x10000DA8) + 0x13000, record i at
;;;; +0x100 + 0x80*i, and the record index is the CPU ID the slot answers to
;;;; (PERCPU-REF 16): it picks the per-CPU active-region cell, the collector token and
;;;; the mutex owner.  Records:
;;;;   0      the main thread (core 0, region 0)
;;;;   1..3   core 1..3's scheduler
;;;;   4..15  thread slots
;;;; Collector words: +0x38 window delta, +0x40 region, +0x68 GC state (0 out,
;;;; 1 parked, 2 safe, 3 running), +0x70 published SP, +0x78 stack top.  A slot that
;;;; is switched out is SAFE with its SP published (the switch stub pushes every
;;;; register a Lisp caller may hold first), so a region-0 stop-the-world scans it,
;;;; and so does a collection of its core's own region (translate-aarch64's SIBLING
;;;; mode: the records whose region is the collector's).
;;;; Bare words: +0x08 alive (1 from start to finish), +0x10 joined, +0x20 home core,
;;;; +0x18 result (raw word, or byte count), +0x28 result kind (1 immediate, 2 bytes, 3 none), +0x30 wait address, +0x48 metadata
;;;; scale, +0x50 wait value, +0x58 deadline (CNTVCT ticks, 0 = none), +0x60 run state
;;;; (0 free, 1 runnable, 2 running, 3 blocked, 4 done).
;;;;
;;;; MEMORY (free RAM per the Pi map; the threaded build moves the GC bitmaps to
;;;; 0x08000000 / 0x08400000 so they cover the thread heaps):
;;;;   0x05200000..0x05800000  scheduler stacks, 2 MB per core
;;;;   0x05800000..0x05E00000  slot stacks, 512 KB each
;;;;   0x10080000..0x10090000  per-CPU blocks, records 0..3;  0x10610000.. slots
;;;;   0x10090000..            region control blocks (core c at +64*c)
;;;;   0x10200000              thread page (table at +0x13000)
;;;;   0x10300000..0x10380000  sync-cell arena (cacheable: LDAXR/STLXR)
;;;;   0x10400000..0x10600000  per-thread windows, 0x20000 per record
;;;;   0x10640000              per-core switch mailboxes, 32 bytes each
;;;;   0x10740000..0x10780000  per-record dynamic-binding stacks, 16 KB each
;;;;   0x10680000..0x10740000  per-slot result buffers, 64 KB each
;;;;   0x18000000..0x1B000000  core heaps: core c = two 8 MB semispaces (free once a
;;;;                           saved core is restored; its magic word is cleared)
;;;;
;;;; WHAT A THREAD MAY DO.  Allocate (in its core's heap), read anything, call compiled
;;;; code, block.  NOT YET: store its own objects into region-0 structures -- a fresh
;;;; INTERN, SETQ of a global, a special binding, SETQ of a variable main closed over
;;;; -- that needs the runtime lock and arena.  Results come back by COPY (encoded
;;;; with modus-channel's encoder), never by pointer.  Threads are started from the
;;;; main thread.  No preemption: a slot that never blocks keeps its core, and one
;;;; that never allocates holds up a region-0 collection until it does.
;;;; PREEMPTION: a slot runs for a 10 ms slice (a CNTVCT deadline in its per-CPU block
;;;; at +0x3F00); past it, its next loop back-edge clamps its allocation limit and its
;;;; next allocation yields the core from inside the collector entry.  A loop that
;;;; neither allocates nor blocks still keeps its core.

(defun %bare-sync-arena () (values #x10300000 #x10380000))  ; cacheable RAM: LDAXR/STLXR

(defun %bt-page () #x10200000)
(defun %bt-table () (+ (%bt-page) #x13000))
(defun %bt-rec (i) (+ (%bt-table) #x100 (* i #x80)))
(defun %bt-window (i) (+ #x10400000 (* i #x20000)))
(defun %bt-percpu (i) (if (< i 4) (+ #x10080000 (* i #x4000)) (+ #x10600000 (* i #x4000))))
(defun %bt-stack-top (i)
  (if (< i 4)
      (+ #x05400000 (* (- i 1) #x200000))
      (+ #x05800000 (* (- i 3) #x80000))))
(defun %bt-rcb (c) (+ #x10090000 (* c 64)))
(defun %bt-mailbox (c) (+ #x10640000 (* c 32)))
(defun %bt-result-buf (i) (+ #x10680000 (* (- i 4) #x10000)))
(defun %bt-result-cap () #xFFF0)
(defun %bt-ncores () 4)
(defun %bt-first-slot () 4)
(defun %bt-nrecs () 16)

(defvar *bt-region-size* nil)
(defun %bt-region-size () (or *bt-region-size* #x800000))   ; 3 cores x 2 x 8 MB = 48 MB
(defun %bt-arena () #x18000000)
(defun %bt-guard (size) (min #x800000 (ash size -2)))       ; the trampoline's own rule

;; The closures threads are started with, by record: a GLOBAL, so a region-0
;; collection between the start and the slot's first instruction updates it.
(defvar *bt-funs* nil)

(defun %bt-stub () (mem-ref #x10213000 :u64))              ; table+0x00
(defun %bt-up-p () (not (zerop (mem-ref #x10213008 :u32))))

;;; Per-computation state, as hosted threads have it (net/hosted-sync.lisp): every
;;; slot runs its body inside %WITH-COMPUTATION-STATE, so the condition system,
;;; the interpreter's MV/NLX state and the printer's scratch are its own.
(defvar *per-computation-specials* nil
  "Globals whose value is PER-COMPUTATION state -- a library's random
   generator, a lazily built cache -- registered by the library with
   REGISTER-PER-COMPUTATION-SPECIAL.  Every thread and actor starts with its
   own NIL binding of each, so the library's (or *x* (setf *x* ...)) fills
   that computation's copy.  Threads share no state: without this, the first
   worker to fill such a global would be storing one of its own objects into
   shared memory, which the shared-store guard refuses.")

(defun register-per-computation-special (symbol)
  "Declare SYMBOL's global value per-computation state (see
   *PER-COMPUTATION-SPECIALS*).  Call it when the library loads, on the main
   thread; computations started afterwards get their own binding."
  (unless (zerop (%tls-self-base))
    (error "register-per-computation-special: call it from the main thread (at load time)."))
  (unless (member symbol *per-computation-specials*)
    (setq *per-computation-specials* (cons symbol *per-computation-specials*)))
  symbol)

(defmacro %with-computation-state ((serial) &body body)
  "Bind, EMPTY, the runtime's per-computation state: the interpreter's MV and
   NLX serial, the condition system, handler/restart stacks, printer scratch,
   loader depth, CLOS dispatch state and CATCH/THROW's values in flight.  A
   thread (%THR-TRAMPOLINE) and an actor (%AR-ACTOR-ENTRY, in its own window)
   each run their whole computation inside one.  SERIAL seeds the NLX state
   serial so no two computations mint the same one."
  `(let ((*mvm-last-mv* nil)
         (*nlx-state-serial* ,serial)
         (*current-condition* nil)
         (*catch-active* nil)
         (*restart-stack* nil)
         (*handler-bind-stack* nil)
         (*handler-bind-effective-skip* 0)
         (*restart-frame-condition-map* nil)
         (*signal-walk-depth* 0)
         (*restarts-being-invoked* nil)
         (*restart-invoking-p* nil)
         (*restart-case-result* nil)
         (*rc-invoked-restart* nil)
         (*format-iter-escape* nil)
         (*write-object-budget* 0)
         (*%circ-next* 0)
         (*%ppx-stack* nil)
         (*%pp-ctx* nil)
         (*load-error-condition* nil)
         (*%load-depth* 0)
         (*%next-methods* nil)
         (*%current-gf-args* nil)
         (*%current-gf* nil)
         (*%dmc-call-args* nil)
         (*catch-tag* nil)
         (*catch-value* nil)
         (*catch-values* nil)
         (*catch-tags* nil)
         ;; macos-hosting's per-thread eval/JIT state, so a worker's runtime
         ;; compile never writes main's flags (2026-09-30 reconciliation).
         (*%escape-report-busy* nil)
         (*jit-native-ran* nil)
         (*jit-infra-fallback* nil)
         (*e2-active-defun-names* nil)
         (*e2-persist-defuns* nil)
         (*e2-module-defuns* nil)
         (*mvm-eval-no-cache* nil)
         (*jit-inhibit* nil)
         )
     (declare (special *%escape-report-busy* *jit-native-ran* *jit-infra-fallback* *e2-active-defun-names* *e2-persist-defuns* *e2-module-defuns* *mvm-eval-no-cache* *jit-inhibit*
                       *mvm-last-mv* *nlx-state-serial* *current-condition* *catch-active* *restart-stack* *handler-bind-stack* *handler-bind-effective-skip* *restart-frame-condition-map* *signal-walk-depth* *restarts-being-invoked* *restart-invoking-p* *restart-case-result* *rc-invoked-restart* *format-iter-escape* *write-object-budget* *%circ-next* *%ppx-stack* *%pp-ctx* *load-error-condition* *%load-depth* *%next-methods* *%current-gf-args* *%current-gf* *%dmc-call-args* *catch-tag* *catch-value* *catch-values* *catch-tags*))
     (progv *per-computation-specials*
         (make-list (length *per-computation-specials*))
       ,@body)))


;;; ------------------------------------------------------------------ machine code

(defun %bt-poke (addr words)
  (let ((i 0))
    (dolist (w words) (%jit-emit-word32 (+ addr (* i 4)) w) (setq i (+ i 1)))
    addr))

(defun %bt-movz-movk-x0 (a)
  (list (logior #xD2800000 (ash (logand a #xFFFF) 5))
        (logior #xF2A00000 (ash (logand (ash a -16) #xFFFF) 5))))

(defun %bt-stub-install ()
  "Write the core entry stub (S), its parameter block (S+0x800), the release routine
   (S+0x1000), core 0's register snapshot (S+0x1100) and the slot switch (S+0x1200);
   take the snapshot.  Sources: test/rpi-smp/stubs.s, test/rpi-smp/switch.s."
  (let* ((s (%mmap-exec-page 8192))
         (p (+ s #x800)))
    (%bt-poke s (list #x10004000 #xf9400001 #xd51ca201 #xf9400401 #xd51c2041 #xf9400801
                      #xd51c2001 #xd50c871f #xd5033f9f #xd5033fdf #xd539f221 #xb27a0021
                      #xd519f221 #xf9401001 #xd51cc001 #xd518c001 #xd53c1141 #x9275f821
                      #xd51c1141 #xd2a00601 #xd5181041 #xd5033fdf #xf9400c01 #xd51c1001
                      #xd5033fdf #xf9401801 #x9100003f #xf9401c01 #xd518d081 #xf9402001
                      #xd51bd041 #xf940141c #xf9402418 #xf9402819 #xd2a20012 #xd280003a
                      #xf2bbd5ba #xd2800021 #xf9003001 #xf9402c09 #xd280001d #xd280001e
                      #xd63f0120 #x10003aa0 #xd2800041 #xf9003001 #xd503205f #x17ffffff))
    (%bt-poke (+ s #x1000)
              (append (%bt-movz-movk-x0 s)
                      (list #x91240002 #xd50b7e20 #x91010000 #xeb02001f #x54ffffa3
                            #xd2801800 #xd50b7e20 #xd5033f9f #xd503209f #xd65f03c0)))
    (%bt-poke (+ s #x1100)
              (append (%bt-movz-movk-x0 p)
                      (list #xd53ca201 #xf9000001 #xd53c2041 #xf9000401 #xd53c2001
                            #xf9000801 #xd53c1001 #xf9000c01 #xd53cc001 #xf9001001
                            #xf900141c #xd65f03c0)))
    (%bt-poke (+ s #x1200)
              (list #xd10203ff #xa9000be1 #xa90113e3 #xa9021be5 #xa90323e7 #xa90453f3
                    #xa9055bf5 #xa9066ff7 #xa9077bfd #xd53800a9 #x92400529 #xd2a20c8a
                    #x8b09154a #xa940314b #xa941394d #x910003ef #xf900396f #xf94001af
                    #xd37ffb10 #xf90019f0 #xb4000079 #xd37ffb30 #xf9001df0 #xd5033bbf
                    #xd2800050 #xf9003570 #xf9401d8f #xd51bd04f #xf940398f #x910001ff
                    #xf94001cf #xf94019f8 #xd341ff18 #xf9401df9 #xd341ff39 #xd2a20010
                    #xf281f910 #xd280006f #xf900358f #xd5033bbf #xc8dffe11 #xb4000111
                    #xd280004f #xd5033bbf #xf900358f #xd503203f #xc8dffe11 #xb5ffffd1
                    #x17fffff5 #xa9400be1 #xa94113e3 #xa9421be5 #xa94323e7 #xa94453f3
                    #xa9455bf5 #xa9466ff7 #xa9477bfd #x910203ff #xd2800000 #xd65f03c0))
    ;; PREEMPT (S+0x1400, test/rpi-smp/preempt.s): called by the collector entry when a
    ;; slot's time slice is up -- mark it runnable, clear its deadline, tail-call the
    ;; switch to its core's scheduler.  Its address at table+0x18 (0x10213018).
    (%bt-poke (+ s #x1400)
              (list #xd53bd049 #xd2a2000a #xf29ffd0a #x8b09014a #xf940014a #xf91f815f
                    #xf940094b #xd341fd6b #xd2a2042c #xf286200c #x8b0b1d8d #xb94021ae
                    #x8b0e1d8f #x52800030 #xb90061b0 #xd2a20c91 #x8b0e1631 #xa9003e2d
                    #xd2a20010 #xf281e110 #x8b0b0e09 #x8b0e0e0a #xa9012a29 #x17ffff69))
    (%jit-icache-flush s 8192)
    (%jit-call (+ s #x1100))
    (setf (mem-ref #x10213000 :u64) s)
    (%gc-write64 #x10213018 (+ s #x1400))
    s))

(defun %bt-entry (sym) (- (%val->word (symbol-function sym)) 3))

(defun %bt-core-id ()
  "The core this code runs on: the home core of the running record."
  (let ((i (percpu-ref 16)))
    (if (< i 4) i (mem-ref (+ (%bt-rec i) #x20) :u32))))

(defun %bt-switch (from to)
  "Leave record FROM (published SAFE) for record TO, on this core.  Returns when
   something switches back to FROM."
  (let ((mb (%bt-mailbox (%bt-core-id))))
    (%gc-write64 mb (%bt-rec from))
    (%gc-write64 (+ mb 8) (%bt-rec to))
    (%gc-write64 (+ mb 16) (+ #x10000F08 (* 8 from)))   ; their ACTIVE regions: a slot
    (%gc-write64 (+ mb 24) (+ #x10000F08 (* 8 to)))     ; in a locked section is in a slice
    (%jit-call (+ (%bt-stub) #x1200))
    0))

;;; ------------------------------------------------------------------ records

(defun %bt-prepare-record (i core sp-entry)
  "A fresh window, per-CPU block and record for record I on CORE.  SP-ENTRY: NIL for
   a scheduler (started by the entry stub); for a slot, the raw entry address the
   switch's RET will land on -- its stack gets the switch's 128-byte frame with x30
   = that address."
  (let ((w (%bt-window i)) (pc (%bt-percpu i)) (rec (%bt-rec i))
        (top (%bt-stack-top i)) (rcb (%bt-rcb core)))
    (dotimes (k 8704) (%gc-write64 (+ w (* k 8)) 0))
    (dotimes (k 2048) (%gc-write64 (+ pc (* k 8)) 0))
    (%gc-write64 (+ w #xFFE8) pc)
    ;; The window's self slot = its delta (%TLS-SELF-BASE), so %DYNBIND binds this
    ;; thread's specials in its own storage, and its dynamic-binding stack.
    (%gc-write64 (+ w #xC30) (- w #x10000000))
    ;; THE SHARED-STORE GUARD'S WORDS: this record's heap is its core's region pair, so
    ;; a store of one of its objects anywhere else traps (on bare metal: !!FAULT and
    ;; the core halts -- loud, not a silent dangling pointer), and the SB-THREAD shim's
    ;; %SB-WORKER-OBJECT-P knows what to copy when a value leaves the thread.
    (let ((a (+ (%bt-arena) (* (- core 1) 2 (%bt-region-size)))))
      (%gc-write64 (+ w #x5040) a)
      (%gc-write64 (+ w #x5048) (+ a (%bt-region-size)))
      (%gc-write64 (+ w #x5050) (%bt-region-size)))
    (setf (mem-ref (+ w #xC60) :u64) (+ #x10740000 (* i #x4000)))
    (setf (mem-ref (+ pc 16) :u64) i)                 ; CPU id = record index
    (%gc-write64 (+ #x10000F08 (* 8 i)) rcb)          ; its active region: the core's
    (dotimes (k 16) (%gc-write64 (+ rec (* k 8)) 0))
    (setf (mem-ref (+ rec #x20) :u32) core)
    (%gc-write64 (+ rec #x38) (- w #x10000000))
    (%gc-write64 (+ rec #x40) rcb)
    (%gc-write64 (+ rec #x48) 2)
    (%gc-write64 (+ rec #x78) top)
    (when sp-entry
      (let ((f (- top 128)))
        (dotimes (k 16) (%gc-write64 (+ f (* k 8)) 0))
        (%gc-write64 (+ f 120) sp-entry)              ; x30
        (%gc-write64 (+ rec #x70) f)
        (%gc-write64 (+ rec #x68) 2)))
    rec))

(defun %bt-carve (c)
  "Core C's heap: [arena + (C-1)*2*SIZE, +2*SIZE), two semispaces, above region 0.
   The threaded build's bitmaps cover it; region 0 is not touched."
  (let* ((size (%bt-region-size))
         (from (+ (%bt-arena) (* (- c 1) 2 size))))
    (%gc-region-init (%bt-rcb c) from (+ from size) size (%bt-stack-top c) 2)
    1))

(defun %bt-core-boot (c)
  "Release core C into its scheduler.  1 once it reports, 0 if it never does."
  (let* ((s (%bt-stub)) (p (+ s #x800)) (rcb (%bt-rcb c)))
    (%bt-carve c)
    (%bt-prepare-record c c nil)
    (%gc-write64 (+ p 48) (%bt-stack-top c))
    (%gc-write64 (+ p 56) (%bt-percpu c))
    (%gc-write64 (+ p 64) (- (%bt-window c) #x10000000))
    (%gc-write64 (+ p 72) (%gc-meta-read rcb 2))
    (%gc-write64 (+ p 80) (- (+ (%gc-meta-read rcb 2) (%gc-meta-read (+ rcb #x10) 2))
                             (%bt-guard (%gc-meta-read (+ rcb #x10) 2))))
    (%gc-write64 (+ p 88) (%bt-entry '%bt-core-main))
    (%gc-write64 (+ p 96) 0)
    (%gc-write64 (+ #xD8 (* 8 c)) s)
    (%jit-call (+ s #x1000))
    (let ((st 0))
      (dotimes (k 20000000)
        (setq st (mem-ref (+ p 96) :u32))
        (when (> st 0) (return nil)))
      (if (= st 1) 1 0))))

(defun %bt-init ()
  "Turn the layer on and start cores 1..3.  1, or 0 if a core did not start."
  (if (%bt-up-p)
      1
      (let ((tt (%bt-table)) (r0 (%gc-region-0)) (ok 1))
        (dotimes (k 512) (%gc-write64 (+ tt (* k 8)) 0))
        (dotimes (k 1024) (%gc-write64 (+ #x10090000 (* k 8)) 0))
        (dotimes (k 16) (%gc-write64 (+ #x10640000 (* k 8)) 0))
        (setf (mem-ref #x10080010 :u64) 0)            ; core 0 is CPU 0
        (setf (mem-ref #x10000FF8 :u32) 1)            ; per-CPU region cells on
        (%gc-write64 (%bt-arena) 0)                   ; a saved core's magic: ours now
        (setq *bt-funs* (make-array (%bt-nrecs)))
        (%bt-stub-install)
        (%gc-write64 #x10000DA8 (%bt-page))
        (%gc-write64 (+ tt #x58) #x10086000)          ; band: rcb i = band+0xA000+64i
        (%gc-write64 (+ tt #x60) (%bt-ncores))
        (let ((rec0 (%bt-rec 0)))
          (%gc-write64 (+ rec0 #x00) 1)
          (%gc-write64 (+ rec0 #x38) 0)
          (%gc-write64 (+ rec0 #x78) (%gc-meta-read (+ r0 #x18) 2))
          (%gc-write64 (+ rec0 #x68) 3))
        (mfence)
        (%gc-write64 (+ tt #x50) 1)                   ; stop-the-world ARMED
        (when (zerop (%rt-threads-on)) (setq ok 0))   ; the runtime lock + arena
        (do ((c 1 (+ c 1))) ((>= c (%bt-ncores)))
          (when (zerop (%bt-core-boot c)) (setq ok 0)))
        (setf (mem-ref #x10213008 :u32) 1)
        ok)))

;;; ------------------------------------------------------------------ the scheduler

(defun %bt-ticks () (rdtsc))
(defun %bt-ms-ticks (ms) (floor (* ms (cntfrq)) 1000))

(defun %bt-ready-p (rec)
  "Whether the slot whose record is REC may run now; a blocked one whose condition
   came true becomes runnable."
  (let ((st (mem-ref (+ rec #x60) :u32)))
    (cond ((= st 1) t)
          ((= st 3)
           (let ((addr (%gc-read64 (+ rec #x30)))
                 (dl (%gc-read64 (+ rec #x58))))
             (when (or (and (> addr 0)
                            (/= (mem-ref addr :u32) (mem-ref (+ rec #x50) :u32)))
                       (and (> dl 0) (>= (%bt-ticks) dl)))
               (setf (mem-ref (+ rec #x60) :u32) 1)
               t)))
          (t nil))))

(defun %bt-pick (c last)
  "The next ready slot on core C after LAST (round robin), or -1."
  (let ((n (- (%bt-nrecs) (%bt-first-slot))) (r -1) (k 1))
    (loop
      (when (or (> k n) (>= r 0)) (return nil))
      (let* ((i (+ (%bt-first-slot) (mod (+ (- last (%bt-first-slot)) k) n)))
             (rec (%bt-rec i)))
        (when (and (= (mem-ref (+ rec #x20) :u32) c) (%bt-ready-p rec))
          (setq r i)))
      (setq k (+ k 1)))
    r))

(defun %bt-core-main ()
  "A core's scheduler: run the ready slots pinned here, round robin, forever.  Idle,
   it is SAFE, so a region-0 collection never waits for it."
  (let* ((c (percpu-ref 16)) (last (%bt-first-slot)))
    (%gc-write64 (+ (%bt-rec c) #x00) 1)
    (loop
      (%gc-safe-enter)
      (let ((s -1))
        (loop (setq s (%bt-pick c last)) (when (>= s 0) (return nil)))
        (%gc-safe-leave)
        (setq last s)
        (setf (mem-ref (+ (%bt-rec s) #x60) :u32) 2)
        ;; ITS TIME SLICE: a CNTVCT deadline the slot's loops and allocations check
        ;; (translate-aarch64 STW-CLAMP / STW-ENTRY); past it, it yields at its next
        ;; allocation.
        (%gc-write64 (+ (%bt-percpu s) #x3F00)
                     (if (zerop (mem-ref (%bt-no-preempt-addr) :u32))
                         (+ (%bt-ticks) (%bt-quantum))
                         0))
        (%bt-switch c s)))))

(defun %bt-quantum () (floor (cntfrq) 100))   ; 10 ms
(defun %bt-no-preempt-addr () (+ (%bt-table) #x20))  ; non-zero: slices never end (tests)

(defun %bt-slot-p () (>= (percpu-ref 16) (%bt-first-slot)))

(defun %bt-block (addr val deadline)
  "Block this slot until the u32 at ADDR is not VAL (ADDR 0: no condition) or the
   CNTVCT DEADLINE passes (0: none), running other slots meanwhile."
  (let* ((i (percpu-ref 16)) (rec (%bt-rec i)))
    (%gc-write64 (+ rec #x30) addr)
    (setf (mem-ref (+ rec #x50) :u32) val)
    (%gc-write64 (+ rec #x58) deadline)
    (setf (mem-ref (+ rec #x60) :u32) 3)
    (%bt-switch i (mem-ref (+ rec #x20) :u32))
    0))

(defun bt-yield ()
  "Let the other ready slots on this core run.  A no-op off a slot."
  (when (%bt-slot-p)
    (let* ((i (percpu-ref 16)) (rec (%bt-rec i)))
      (setf (mem-ref (+ rec #x60) :u32) 1)
      (%bt-switch i (mem-ref (+ rec #x20) :u32))))
  nil)

(defun %bt-encoder (name) (symbol-function (intern name "COMMON-LISP-USER")))

(defun %bt-publish-result (i v)
  "V into slot I's result: an immediate as its raw word in +0x18 (kind 1), anything
   else as modus-channel bytes in the slot's result buffer, length at +0x18 (kind 2),
   or kind 3 when it cannot be encoded or does not fit.  The kind is at +0x28."
  (let ((rec (%bt-rec i)) (buf (%bt-result-buf i)) (kind 3))
    (if (or (typep v 'fixnum) (characterp v) (eq v nil) (eq v t))
        (progn (setf (mem-ref (+ rec #x18) :u64) v) (setq kind 1))
        (handler-case
            (let* ((out (funcall (%bt-encoder "%CHAN-ENCODE") v))
                   (bytes (car out)) (len (cdr out)))
              (when (<= len (%bt-result-cap))
                (dotimes (k len) (setf (mem-ref (+ buf k) :u8) (aref bytes k)))
                (%gc-write64 (+ rec #x18) len)
                (setq kind 2)))
          (error () (setq kind 3))))
    (%gc-write64 (+ rec #x28) kind)))

(defun %bt-slot-main ()
  "Every slot starts here, on its own stack, by the switch's RET."
  (let* ((i (percpu-ref 16)) (rec (%bt-rec i)))
    (%gc-write64 (+ rec #x00) 1)
    (%with-computation-state ((* i 1099511627776))
      (let* ((fn (svref *bt-funs* i))
             (v (handler-case (funcall fn) (error () :error))))
        (setf (svref *bt-funs* i) nil)
        (%bt-publish-result i v)))
    (mfence)
    (setf (mem-ref (+ rec #x60) :u32) 4)
    (setf (mem-ref (+ rec #x08) :u32) 0)
    (loop (%bt-switch i (mem-ref (+ rec #x20) :u32)))))

;;; ------------------------------------------------------------------ starting, joining

(defun %bt-free-slot ()
  (let ((r -1))
    (do ((i (%bt-first-slot) (+ i 1))) ((or (>= i (%bt-nrecs)) (>= r 0)))
      (let ((st (mem-ref (+ (%bt-rec i) #x60) :u32)))
        (when (or (= st 0)
                  (and (= st 4) (not (zerop (mem-ref (+ (%bt-rec i) #x10) :u32)))))
          (setq r i))))
    r))

(defun %bt-least-loaded-core ()
  (let ((best 1) (bestn 1000))
    (do ((c 1 (+ c 1))) ((>= c (%bt-ncores)))
      (let ((n 0))
        (do ((i (%bt-first-slot) (+ i 1))) ((>= i (%bt-nrecs)))
          (let ((rec (%bt-rec i)))
            (when (and (= (mem-ref (+ rec #x20) :u32) c)
                       (not (zerop (mem-ref (+ rec #x08) :u32))))
              (setq n (+ n 1)))))
        (when (< n bestn) (setq best c bestn n))))
    best))

(defun %bt-own-object-p (x)
  "Whether X is a heap object in the calling thread's own heap (its window's region
   pair); NIL on the main thread."
  (let ((self (%tls-self-base)))
    (if (zerop self)
        nil
        (let ((w (%val->word x)))
          (and (= (logand w 5) 1)
               (let ((a (logand w -16))
                     (ba (%gc-read64 (+ self #x10005040)))
                     (bb (%gc-read64 (+ self #x10005048)))
                     (sz (%gc-read64 (+ self #x10005050))))
                 (and (> sz 0)
                      (or (and (>= a ba) (< a (+ ba sz)))
                          (and (>= a bb) (< a (+ bb sz)))))))))))

(defun %bt-spawn-lock () (+ (%bt-table) #x10))

(defun %make-native-thread (fn &rest args)
  "Start FN (no arguments) as a thread on the least-loaded core.  Its record number
   (4..15), or -1 when every slot is in use, -2 when the cores did not start, -7 when
   a thread passes a function object made in its own heap (it cannot outlive that
   heap's next collection in a shared table: pass #'NAME, or let SB-THREAD:MAKE-THREAD
   carry it).  Threads may start threads."
  (declare (ignore args))
  (cond ((zerop (%bt-init)) -2)
        ((%bt-own-object-p fn) -7)
        (t
         (when (and (zerop (%thr-cpu)) (fboundp 'jit-eager))
           (handler-case (jit-eager) (error () nil)))
         (let ((lk (%bt-spawn-lock)))
           (if (%bt-slot-p)
               (loop (when (zerop (xchg-mem lk 1)) (return nil)) (bt-yield))
               (loop (when (zerop (xchg-mem lk 1)) (return nil))))
           (let ((i (%bt-free-slot)))
             (if (< i 0)
                 (progn (%mutex-unlock lk) -1)
                 (let ((c (%bt-least-loaded-core)))
                   (setf (svref *bt-funs* i) fn)
                   (let ((rec (%bt-prepare-record i c (%bt-entry '%bt-slot-main))))
                     (setf (mem-ref (+ rec #x08) :u32) 1)
                     (mfence)
                     (setf (mem-ref (+ rec #x60) :u32) 1))
                   (%mutex-unlock lk)
                   i)))))))

(defun %native-thread-alive-p (i)
  (if (zerop (mem-ref (+ (%bt-rec i) #x08) :u32)) 0 1))

(defun %join-native-thread (i budget)
  "0 once thread I has ended (its slot is then free), 1 if BUDGET polls ran out."
  (let ((r 1) (k 0))
    ;; Off a slot the wait is SAFE: a region-0 collection started by a thread (the
    ;; lock arena running low) must not wait for a joiner that never allocates.
    (unless (%bt-slot-p) (%gc-safe-enter))
    (loop
      (when (zerop (mem-ref (+ (%bt-rec i) #x08) :u32)) (setq r 0) (return nil))
      (when (>= k budget) (return nil))
      (if (%bt-slot-p) (bt-yield) nil)
      (setq k (+ k 1)))
    (unless (%bt-slot-p) (%gc-safe-leave))
    (when (zerop r) (setf (mem-ref (+ (%bt-rec i) #x10) :u32) 1))
    r))

(defun %bt-result (i)
  "Thread I's value, decoded in the caller's heap; signals when it was not encodable."
  (let* ((rec (%bt-rec i)) (kind (%gc-read64 (+ rec #x28))))
    (cond ((= kind 1) (mem-ref (+ rec #x18) :u64))
          ((= kind 2)
           (let* ((n (%gc-read64 (+ rec #x18))) (buf (%bt-result-buf i))
                  (v (make-array n :element-type '(unsigned-byte 8))))
             (dotimes (k n) (setf (aref v k) (mem-ref (+ buf k) :u8)))
             (funcall (%bt-encoder "%CHAN-DECODE") v nil)))
          (t (error "bt-join: thread ~D's value cannot be copied out of its heap" i)))))

(defun %thr-cpu ()
  (if (= (mem-ref #x10000FF8 :u32) 0) 0 (percpu-ref 16)))

;;; ------------------------------------------------------------------ public

(defun bt-spawn (fn)
  "Start FN (a function of no arguments) as a thread; its handle."
  (let ((i (%make-native-thread fn)))
    (if (< i 0) (error "bt-spawn: no thread could be started (~D)" i) i)))

(defun bt-join (i)
  "Wait for thread I; its value, copied out of its core's heap.  Frees the slot."
  (%join-native-thread i most-positive-fixnum)
  (%bt-result i))

(defun bt-status ()
  "Per record: (alive run-state core gc-state region-collections)."
  (loop for i from 0 below (%bt-nrecs)
        collect (let ((rec (%bt-rec i)))
                  (list (mem-ref (+ rec #x08) :u32) (mem-ref (+ rec #x60) :u32)
                        (mem-ref (+ rec #x20) :u32) (%gc-read64 (+ rec #x68))
                        (let ((rcb (%gc-read64 (+ rec #x40))))
                          (if (zerop rcb) 0 (%gc-meta-read (+ rcb #x20) 2)))))))

;;; ------------------------------------------------------------------ blocking primitives

(defun %mutex-trylock (cell) (if (zerop (xchg-mem cell 1)) 1 0))

(defun %mutex-lock (cell)
  "Spin on the main thread and the schedulers; BLOCK on a slot (until the word moves)."
  (loop
    (when (zerop (xchg-mem cell 1)) (return 1))
    (if (%bt-slot-p)
        (%bt-block cell 1 0)
        (progn (%gc-safe-enter)
               (loop (when (zerop (mem-ref cell :u32)) (return nil)))
               (%gc-safe-leave)))))

(defun %mutex-unlock (cell) (mfence) (xchg-mem cell 0) 0)

(defun %sleep-ms (ms)
  (let ((dl (+ (%bt-ticks) (%bt-ms-ticks ms))))
    (if (%bt-slot-p)
        (%bt-block 0 0 dl)
        (progn (%gc-safe-enter)
               (loop (when (>= (%bt-ticks) dl) (return nil)))
               (%gc-safe-leave))))
  nil)

(defun %monotonic-ns () (%irt-ns))

(defun %cond-bump (cv)
  "Advance condition word CV (the mutex is held by the caller, so a plain add)."
  (setf (mem-ref cv :u32) (logand (+ (mem-ref cv :u32) 1) #xFFFFFFF))
  0)

(defun %futex-wake (addr n) (declare (ignore addr n)) 0)   ; waiters poll the word

(defun %cond-wait-ms (cv mtx ms)
  "Wait for CV to be bumped (or MS milliseconds; 0 = no timeout) with MTX released;
   re-take MTX.  0 when signalled, 1 on timeout."
  (let* ((seq (mem-ref cv :u32))
         (dl (if (> ms 0) (+ (%bt-ticks) (%bt-ms-ticks ms)) 0))
         (r 0))
    (%mutex-unlock mtx)
    (if (%bt-slot-p)
        (%bt-block cv seq dl)
        (progn (%gc-safe-enter)
               (loop (when (/= (mem-ref cv :u32) seq) (return nil))
                     (when (and (> dl 0) (>= (%bt-ticks) dl)) (return nil)))
               (%gc-safe-leave)))
    (when (= (mem-ref cv :u32) seq) (setq r 1))
    (%mutex-lock mtx)
    r))
;;; ------------------------------------------------------------------ the runtime lock
;;;
;;; THE RUNTIME LOCK AND THE LOCK ARENA -- hosted's B-LITE (net/hosted-sync.lisp, read
;;; its block) with a spinlock: a slot that finds it held BLOCKS (%MUTEX-LOCK), the
;;; main thread spins.  The shared tables (globals, symbols, keywords, packages,
;;; macros) live in region 0; a locked section (%RT-ENTER .. %RT-LEAVE, which
;;; SYMBOL-VALUE, INTERN and friends take) allocates in this CPU's SLICE of an
;;; ARENA carved off the top of region 0's semispaces, so what it makes is region-0
;;; address space and never in a thread's heap.  A stop-the-world collection of
;;; region 0 evacuates the arena when the lock is free (translate-aarch64, EVACUATING
;;; THE LOCK ARENA), and an outermost entry that finds under a quarter left starts one.
;;; With the gate open, a slot's special bindings are its own (%DYNBIND: the window's
;;; self slot is set by %BT-PREPARE-RECORD).
;;;   0x10000DB8 gate  0x10000DC0 lock  DC8 owner (cpu+1)  DD0 depth  DD8 saved region
;;;   band = 0x10086000: +0xB800 + 64*cpu slice blocks, +0xBC00 arena base, frontier,
;;;   end, fallbacks, +0xBC20 refill cooldown.

(defvar *ha-band* #x10086000)
(defun %ha-base () #x10086000)
(defun %rt-arena-bytes () #x400000)            ; 4 MB off each of region 0's semispaces

(defun %ha-zero (start end)
  (let ((a start))
    (loop (when (>= a end) (return 0)) (setf (mem-ref a :u64) 0) (setq a (+ a 8)))))

(defun %ha-zero-bytes (start end)
  (let ((a start))
    (loop (when (or (>= a end) (zerop (logand a 7))) (return 0))
          (setf (mem-ref a :u8) 0) (setq a (+ a 1)))
    (let ((w (logand end -8))) (when (> w a) (%ha-zero a w) (setq a w)))
    (loop (when (>= a end) (return 0)) (setf (mem-ref a :u8) 0) (setq a (+ a 1)))))

(defun %ha-bitmap-clear (lo hi)
  (let ((pb (%gc-bitmap-page-base-exact)))
    (when (and (> (%gc-bitmap-base) 0) (> (%gc-cons-bitmap-base) 0) (>= lo pb) (> hi lo))
      (let ((b0 (ash (- lo pb) -7)) (b1 (ash (+ (- hi pb) 127) -7)))
        (%ha-zero-bytes (+ (%gc-bitmap-base) b0) (+ (%gc-bitmap-base) b1))
        (%ha-zero-bytes (+ (%gc-cons-bitmap-base) b0) (+ (%gc-cons-bitmap-base) b1))))
    0))

(defun %rt-gate-addr ()  #x10000DB8)
(defun %rt-mutex-addr () #x10000DC0)
(defun %rt-owner-addr () #x10000DC8)
(defun %rt-depth-addr () #x10000DD0)
(defun %rt-saved-addr () #x10000DD8)
(defun %rt-slice-block (cpu) (+ (%ha-base) #xB800 (* cpu #x40)))
(defun %rt-arena-words () (+ (%ha-base) #xBC00))
(defun %rt-arena-base () (%gc-read64 (%rt-arena-words)))
(defun %rt-arena-alloc () (%gc-read64 (+ (%rt-arena-words) #x08)))
(defun %rt-arena-end () (%gc-read64 (+ (%rt-arena-words) #x10)))
(defun %rt-arena-fallbacks () (%gc-read64 (+ (%rt-arena-words) #x18)))

(defun %rt-slice-need ()
  (let ((self (%tls-self-base)))
    (if (zerop self)
        #x10000
        (let ((n (%gc-read64 (+ self #x100050A8)))) (if (> n #x10000) n #x10000)))))

(defun %rt-enter-sized (bytes)
  (let ((self (%tls-self-base)))
    (unless (zerop self) (%gc-write64 (+ self #x100050A8) (+ bytes 64))))
  (%rt-enter)
  (let ((self (%tls-self-base)))
    (unless (zerop self) (%gc-write64 (+ self #x100050A8) 0))))

(defun %rt-slice-ensure (need)
  "This CPU's slice block with NEED bytes of headroom, refilled from the arena (256 KB
   chunks, or what NEED takes); 0 when the arena is missing or exhausted (counted)."
  (let ((ae (%rt-arena-end)))
    (if (zerop ae)
        0
        (let* ((k 2) (blk (%rt-slice-block (%thr-cpu)))
               (alloc (%gc-meta-read (+ blk #x30) k))
               (limit (%gc-meta-read (+ blk #x38) k)))
          (if (>= (- limit alloc) need)
              blk
              (let* ((af (%rt-arena-alloc))
                     (chunk (if (> (+ need #x10000) #x40000)
                                (logand (+ need #x10000 #xFFFF) (- 0 #x10000))
                                #x40000)))
                (if (> (+ af chunk) ae)
                    (progn (%gc-write64 (+ (%rt-arena-words) #x18) (+ (%rt-arena-fallbacks) 1))
                           0)
                    (progn (%gc-region-init blk af af chunk 0 k)
                           (%gc-write64 (+ (%rt-arena-words) #x08) (+ af chunk))
                           blk))))))))

(defun %rt-reserve (bytes)
  (if (or (= (mem-ref #x10000DB8 :u32) 0)
          (/= (%gc-read64 (%rt-owner-addr)) (+ (%thr-cpu) 1))
          (zerop (%rt-arena-end)))
      0
      (let ((blk (%rt-slice-block (%thr-cpu))))
        (if (or (/= (%gc-region) blk)
                (>= (- (get-alloc-limit) (get-alloc-ptr)) bytes))
            0
            (progn
              (%gc-region-enter (%gc-region-0))
              (let ((b (%rt-slice-ensure (+ bytes #x10000))))
                (%gc-region-enter (if (zerop b) blk b)))
              0)))))

(defun %rt-own-alloc-begin ()
  (if (or (= (mem-ref #x10000DB8 :u32) 0) (= (%thr-cpu) 0)
          (/= (%gc-read64 (%rt-owner-addr)) (+ (%thr-cpu) 1)))
      0
      (let ((own (%gc-read64 (%rt-saved-addr))))
        (if (zerop own) 0 (%gc-region-enter own)))))

(defun %rt-own-alloc-end (token)
  (if (zerop token) 0 (progn (%gc-region-enter token) 0)))

(defun %gc-collect-region-0 ()
  "Collect region 0 now from any thread (stop-the-world, evacuating the arena)."
  (let ((k 2) (r0 (%gc-region-0)) (own (%gc-region)))
    (if (= own r0)
        (progn (set-alloc-limit (get-alloc-ptr)) (%gc-collect-now) 0)
        (let ((a (get-alloc-ptr)) (l (get-alloc-limit)))
          (%gc-meta-write (+ own #x30) a k)
          (%gc-set-region r0)
          (set-alloc-limit a)
          (%gc-collect-now)
          (%gc-set-region own)
          (set-alloc-ptr a)
          (set-alloc-limit l)
          0))))

(defun %rt-arena-refill-check ()
  (let ((ae (%rt-arena-end)))
    (when (> ae 0)
      (let* ((base (%rt-arena-base)) (quarter (ash (- ae base) -2)))
        (when (< (- ae (%rt-arena-alloc)) quarter)
          (let ((cool (%gc-read64 (+ (%rt-arena-words) #x20))))
            (if (and (> cool 0) (>= (- ae (%rt-arena-alloc)) (ash quarter -1)))
                (%gc-write64 (+ (%rt-arena-words) #x20) (- cool 1))
                (progn
                  (%gc-collect-region-0)
                  (when (< (- ae (%rt-arena-alloc)) quarter)
                    (%gc-write64 (+ (%rt-arena-words) #x20) 256)))))))))
  0)

(defun %rt-arena-carve ()
  "The lock arena: the top %RT-ARENA-BYTES of region 0's semispaces, once (after a
   collection when the frontier is in the way).  1 = ready."
  (if (> (%rt-arena-end) 0)
      1
      (let* ((k 2) (r0 (%gc-region-0)) (n (%rt-arena-bytes)))
        (when (> (get-alloc-ptr)
                 (- (+ (%gc-meta-read r0 k) (- (%gc-meta-read (+ r0 #x10) k) (+ n #x200000)))
                    #x800000))
          (%gc-collect-here))
        ;; THE TOP 2 MB OF EACH SEMISPACE IS LEFT OUT: the board's *CSTR-SCRATCH* and
        ;; *IO-BUF-ADDR* live at 0x0FE00000..0x10000000, the top of the UPPER semispace
        ;; (inside its old 8 MB limit guard), and an arena carved there when the upper
        ;; one is the from-space would hand the I/O staging pages out as objects.
        (let* ((from (%gc-meta-read r0 k))
               (size (%gc-meta-read (+ r0 #x10) k))
               (newsize (- size (+ n #x200000)))
               (base (+ from newsize))
               (end (+ base n)))
          (if (> (get-alloc-ptr) (- (+ from newsize) #x800000))
              0
              (progn
                (%ha-zero base end)
                (%ha-bitmap-clear base end)
                (%gc-region-shrink r0 newsize k)
                (set-alloc-limit (- (+ from newsize) #x800000))
                (%ha-zero (%rt-slice-block 0) (+ (%rt-slice-block 0) #x400))
                (%gc-write64 (%rt-arena-words) base)
                (%gc-write64 (+ (%rt-arena-words) #x08) base)
                (%gc-write64 (+ (%rt-arena-words) #x10) end)
                (%gc-write64 (+ (%rt-arena-words) #x18) 0)
                (%gc-write64 (+ (%rt-arena-words) #x20) 0)
                (%gc-write64 #x10000D98 (%rt-arena-words))
                1))))))

(defun %bt-fatal (msg)
  "MSG on the console, then stop this core: a state where going on corrupts."
  (write-string-serial msg)
  (loop))

(defun %rt-enter-locked ()
  (let ((me (+ (%thr-cpu) 1)))
    (if (= (%gc-read64 (%rt-owner-addr)) me)
        (progn (%gc-write64 (%rt-depth-addr) (+ (%gc-read64 (%rt-depth-addr)) 1)) 0)
        (progn
          (unless (zerop (%gc-read64 (%rt-mutex-addr)))
            (%gc-write64 #x10000DE8 (+ (%gc-read64 #x10000DE8) 1)))
          (%rt-arena-refill-check)
          (%mutex-lock (%rt-mutex-addr))
          (%gc-write64 #x10000DE0 (+ (%gc-read64 #x10000DE0) 1))
          (%gc-write64 (%rt-owner-addr) me)
          (%gc-write64 (%rt-depth-addr) 1)
          (let ((blk (%rt-slice-ensure (%rt-slice-need))))
            (when (and (zerop blk) (> (%thr-cpu) 0))
              (%bt-fatal "
modus: a thread needed the runtime lock but the lock arena is exhausted; stopping this core.
"))
            (%gc-write64 (%rt-saved-addr)
                         (%gc-region-enter (if (zerop blk) (%gc-region-0) blk))))
          0))))

(defun %rt-leave-locked ()
  (let ((me (+ (%thr-cpu) 1)))
    (if (= (%gc-read64 (%rt-owner-addr)) me)
        (let ((d (- (%gc-read64 (%rt-depth-addr)) 1)))
          (if (> d 0)
              (progn (%gc-write64 (%rt-depth-addr) d) 0)
              (let ((back (%gc-read64 (%rt-saved-addr))))
                (%gc-region-enter back)
                (%gc-write64 (%rt-depth-addr) 0)
                (%gc-write64 (%rt-owner-addr) 0)
                (%mutex-unlock (%rt-mutex-addr))
                0)))
        0)))

(defun %publish-fence ()
  (if (= (mem-ref #x10000DB8 :u32) 0) 0 (progn (mfence) 0)))

(defun %rt-threads-on ()
  "Carve the lock arena and open the threads gate.  1, or 0 with no arena."
  (if (= (mem-ref (%rt-gate-addr) :u32) 1)
      1
      (progn
        (%gc-write64 (%rt-owner-addr) 0)
        (%gc-write64 (%rt-depth-addr) 0)
        (%gc-write64 (%rt-mutex-addr) 0)
        (%gc-write64 #x10000DE0 0)
        (%gc-write64 #x10000DE8 0)
        (if (zerop (%rt-arena-carve))
            0
            (progn (mfence) (setf (mem-ref (%rt-gate-addr) :u32) 1) 1)))))

(defun %rt-acquisitions () (%gc-read64 #x10000DE0))
(defun %rt-contended () (%gc-read64 #x10000DE8))

(defun %tls-self-base ()
  "This thread's window delta, read from the self slot inside the window (0 on main)."
  (let ((lo (mem-ref #x10000C30 :u32))
        (hi (mem-ref #x10000C34 :u32)))
    (if (= hi 0) lo (+ (* (* hi 65536) 65536) lo))))

;;; ------------------------------------------------------------------ the SB-THREAD shim
;;; net/sb-thread-shim.lisp brings a process up with hosted's actor-band calls; here the
;;; band is fixed and %BT-INIT does all of it.

(defun %ha-carve () (if (= (%bt-init) 1) (%ha-base) 0))
(defun %ha-percpu-base () #x10080000)
(defun %ha-percpu-init-cpu (base cpu) (declare (ignore base cpu)) 0)
(defun %ha-set-percpu-mode (v) (declare (ignore v)) 0)
(defun %ha-percpu-mode () (mem-ref #x10000FF8 :u32))
(defun %ha-nregions () (%bt-ncores))

(defun %cond-signal (cv) (%cond-bump cv) 0)
(defun %cond-broadcast (cv) (%cond-bump cv) 0)
(defun %cond-wait (cv mtx) (%cond-wait-ms cv mtx 0))
(defun %futex-wake-all (addr) (declare (ignore addr)) 0)

(defun %sync-cell ()
  "A zeroed 64-byte cell in the cacheable sync arena, or 0 when it is full.  Handed out
   under a spinlock (arena header +0x10): threads make mutexes too."
  (multiple-value-bind (start end) (%bare-sync-arena)
    (let ((lk (+ start #x10)) (r 0))
      (loop (when (zerop (xchg-mem lk 1)) (return nil)) (bt-yield))
      (let* ((n (mem-ref start :u32))
             (a (+ start 64 (* n 64))))
        (when (<= (+ a 64) end)
          (dotimes (i 8) (%gc-write64 (+ a (* i 8)) 0))
          (setf (mem-ref start :u32) (+ n 1))
          (setq r a)))
      (mfence)
      (xchg-mem lk 0)
      r)))
