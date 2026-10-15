;;;; bare-threads.lisp - native threads on the Pi's secondary cores (MODUS_RPI_THREADS=1).
;;;;
;;;; Linked after net/bare-sync.lisp, whose single-threaded answers it replaces by
;;;; last-defun-wins.  A "thread" is a CORE: cores 1..3 are released from the firmware's
;;;; spin table once (the stub below, test/rpi-smp/stubs.s) into %BT-CORE-MAIN, which never
;;;; returns -- it waits for a job, runs it, reports, and waits again.  So a thread start is
;;;; a store and a handshake, not a boot.
;;;;
;;;; THE CONTRACT IS HOSTED AARCH64'S (net/hosted-sync.lisp), so translate-aarch64's
;;;; stop-the-world collector works unchanged: the thread table is the thread page
;;;; (0x10000DA8) + 0x13000, record i at +0x100 + 0x80*i with i = the CPU id, and a record
;;;; holds +0x38 window delta, +0x40 region, +0x68 GC state (0 out, 1 parked, 2 safe,
;;;; 3 running), +0x70 published SP, +0x78 stack top.  Carved regions are band + 0xA000 +
;;;; 64*i with band = 0x10086000, so region i is 0x10090000 + 64*i -- core i's own.
;;;; Bare-only record words: +0x08 busy (1 from post to finish), +0x10 the job closure,
;;;; +0x18 its result, +0x20 job posted, +0x28 job taken, +0x30 core started, +0x58 joined.
;;;;
;;;; WHAT A WORKER MAY DO (M1).  Allocate (in its own region, which it collects itself),
;;;; read anything, call compiled code, handler-case.  Core 0 may collect region 0 while
;;;; workers run: it stops them at their next allocation (or finds them idle, SAFE) and
;;;; scans their stacks, windows and regions.  NOT YET: anything that stores a worker
;;;; object into a region-0 structure -- interning a fresh symbol, SETQ of a global, a
;;;; special binding (shallow-bound into the globals table), SETQ of a closed-over
;;;; variable.  Those need the runtime lock and the lock arena (hosted's B-lite).
;;;; A loop that never allocates holds a region-0 collection up until it does.

(defun %bare-sync-arena () (values #x10300000 #x10380000))  ; cacheable RAM: LDAXR/STLXR

(defun %bt-page () #x10200000)
(defun %bt-table () (+ (%bt-page) #x13000))
(defun %bt-rec (cpu) (+ (%bt-table) #x100 (* cpu #x80)))
(defun %bt-window (cpu) (+ #x10400000 (* cpu #x20000)))
(defun %bt-percpu (cpu) (+ #x10080000 (* cpu #x4000)))
(defun %bt-stack-top (cpu) (+ #x05400000 (* (- cpu 1) #x200000)))
(defun %bt-rcb (cpu) (+ #x10090000 (* cpu 64)))
(defun %bt-ncores () 4)

;; Bytes per semispace of a core's region.  Its allocation limit sits min(8 MB, size/4)
;; below the end (translate-aarch64's trampoline), so 8 MB leaves a core 6 MB to fill.
(defvar *bt-region-size* nil)
(defun %bt-region-size () (or *bt-region-size* #x800000))  ; 3 cores x 2 x 8 MB = 48 MB

;; Host-free words: the stub page (exec) and whether the layer is up.
(defun %bt-stub () (mem-ref #x10213000 :u64))   ; table+0x00 (unused by the collector)
(defun %bt-up-p () (not (zerop (mem-ref #x10213008 :u32))))

;;; ------------------------------------------------------------------ the core stub

(defun %bt-poke (addr words)
  (let ((i 0))
    (dolist (w words) (%jit-emit-word32 (+ addr (* i 4)) w) (setq i (+ i 1)))
    addr))

(defun %bt-movz-movk-x0 (a)
  (list (logior #xD2800000 (ash (logand a #xFFFF) 5))
        (logior #xF2A00000 (ash (logand (ash a -16) #xFFFF) 5))))

(defun %bt-stub-install ()
  "Write the entry stub (S), its parameter block (S+0x800), the release routine
   (S+0x1000) and core 0's register snapshot routine (S+0x1100); take the snapshot."
  (let* ((s (%mmap-exec-page 8192))
         (p (+ s #x800)))
    ;; ENTRY: MMU/caches as core 0 has them, SMPEN, FP, vectors, then SP, TPIDR_EL1,
    ;; TPIDR_EL0 (window delta), x28, x24/x25, x18, x26 = NIL; status 1; BLR entry.
    (%bt-poke s (list #x10004000 #xf9400001 #xd51ca201 #xf9400401 #xd51c2041 #xf9400801
                      #xd51c2001 #xd50c871f #xd5033f9f #xd5033fdf #xd539f221 #xb27a0021
                      #xd519f221 #xf9401001 #xd51cc001 #xd518c001 #xd53c1141 #x9275f821
                      #xd51c1141 #xd2a00601 #xd5181041 #xd5033fdf #xf9400c01 #xd51c1001
                      #xd5033fdf #xf9401801 #x9100003f #xf9401c01 #xd518d081 #xf9402001
                      #xd51bd041 #xf940141c #xf9402418 #xf9402819 #xd2a20012 #xd280003a
                      #xf2bbd5ba #xd2800021 #xf9003001 #xf9402c09 #xd280001d #xd280001e
                      #xd63f0120 #x10003aa0 #xd2800041 #xf9003001 #xd503205f #x17ffffff))
    ;; RELEASE: clean [S, S+0x900) and the spin-table line to PoC, DSB, SEV.
    (%bt-poke (+ s #x1000)
              (append (%bt-movz-movk-x0 s)
                      (list #x91240002 #xd50b7e20 #x91010000 #xeb02001f #x54ffffa3
                            #xd2801800 #xd50b7e20 #xd5033f9f #xd503209f #xd65f03c0)))
    ;; SNAPSHOT: MAIR/TCR/TTBR0/SCTLR/VBAR (EL2) and x28 into the parameter block.
    (%bt-poke (+ s #x1100)
              (append (%bt-movz-movk-x0 p)
                      (list #xd53ca201 #xf9000001 #xd53c2041 #xf9000401 #xd53c2001
                            #xf9000801 #xd53c1001 #xf9000c01 #xd53cc001 #xf9001001
                            #xf900141c #xd65f03c0)))
    (%jit-icache-flush s 8192)
    (%jit-call (+ s #x1100))
    (setf (mem-ref #x10213000 :u64) s)
    s))

(defun %bt-core-boot (c)
  "Release core C into %BT-CORE-MAIN with its own stack, window, per-CPU block and
   region.  1 once the core reports it is running, 0 if it never does."
  (let* ((s (%bt-stub)) (p (+ s #x800))
         (w (%bt-window c)) (pc (%bt-percpu c)) (rcb (%bt-rcb c)) (rec (%bt-rec c)))
    (dotimes (i 8704) (%gc-write64 (+ w (* i 8)) 0))
    (dotimes (i 2048) (%gc-write64 (+ pc (* i 8)) 0))
    (%gc-write64 (+ w #xFFE8) pc)
    (setf (mem-ref (+ pc 16) :u64) c)                 ; CPU id, read as PERCPU-REF 16
    (%gc-write64 (+ rec #x38) (- w #x10000000))
    (%gc-write64 (+ rec #x40) rcb)
    (%gc-write64 (+ rec #x48) 2)
    (%gc-write64 (+ rec #x78) (%bt-stack-top c))
    (%gc-write64 (+ p 48) (%bt-stack-top c))
    (%gc-write64 (+ p 56) pc)
    (%gc-write64 (+ p 64) (- w #x10000000))
    (%gc-write64 (+ p 72) (%gc-meta-read rcb 2))      ; x24 = the region's from-start
    (%gc-write64 (+ p 80) (- (+ (%gc-meta-read rcb 2) (%gc-meta-read (+ rcb #x10) 2))
                             (%bt-guard (%gc-meta-read (+ rcb #x10) 2))))   ; x25 = its limit
    (%gc-write64 (+ p 88) (- (%val->word (symbol-function '%bt-core-main)) 3))
    (%gc-write64 (+ p 96) 0)
    (%gc-write64 (+ #xD8 (* 8 c)) s)
    (%jit-call (+ s #x1000))
    (let ((st 0))
      (dotimes (i 20000000)
        (setq st (mem-ref (+ p 96) :u32))
        (when (> st 0) (return nil)))
      (if (= st 1)
          (progn (setf (mem-ref (+ rec #x30) :u32) 1) 1)
          0))))

;;; ------------------------------------------------------------------ regions + STW

(defun %bt-arena () #x18000000)       ; thread heaps: [0x18000000, 0x1B000000)
(defun %bt-guard (size) (min #x800000 (ash size -2)))   ; the trampoline's own rule

(defun %bt-carve (c)
  "Core C's region: two semispaces of SIZE out of the thread-heap arena above region 0
   -- core C gets [arena + (C-1)*2*SIZE, +2*SIZE).  The arena is where a saved core is
   loaded (0x18000000), free once it has been restored, and the threaded build's
   bitmaps cover it.  Region 0 is not touched."
  (let* ((size (%bt-region-size))
         (from (+ (%bt-arena) (* (- c 1) 2 size))))
    (%gc-region-init (%bt-rcb c) from (+ from size) size (%bt-stack-top c) 2)
    (%gc-write64 (+ #x10000F08 (* 8 c)) (%bt-rcb c))
    1))

(defun %bt-init ()
  "Turn the layer on: zero the thread table and the carved-region blocks, stamp core 0,
   per-CPU region mode on, install the stub, ARM stop-the-world last."
  (if (%bt-up-p)
      1
      (let ((tt (%bt-table)) (r0 (%gc-region-0)))
        (dotimes (i 512) (%gc-write64 (+ tt (* i 8)) 0))            ; table + 16 records
        (dotimes (i 1024) (%gc-write64 (+ #x10090000 (* i 8)) 0))   ; rcbs, band slices/arena
        (setf (mem-ref #x10080010 :u64) 0)                          ; core 0 is CPU 0
        (setf (mem-ref #x10000FF8 :u32) 1)                          ; per-CPU region cells
        (%gc-write64 (%bt-arena) 0)            ; the saved core's magic: the arena is ours now
        (%bt-stub-install)
        (%gc-write64 #x10000DA8 (%bt-page))
        (%gc-write64 (+ tt #x58) #x10086000)                        ; band: rcb i = +0xA000+64i
        (%gc-write64 (+ tt #x60) (%bt-ncores))
        (let ((rec0 (%bt-rec 0)))
          (%gc-write64 (+ rec0 #x00) 1)
          (%gc-write64 (+ rec0 #x38) 0)
          (%gc-write64 (+ rec0 #x78) (%gc-meta-read (+ r0 #x18) 2))
          (%gc-write64 (+ rec0 #x68) 3))
        (mfence)
        (%gc-write64 (+ tt #x50) 1)
        (setf (mem-ref #x10213008 :u32) 1)
        1)))

;;; ------------------------------------------------------------------ the worker side

(defun %bt-wait-job (rec)
  (loop (unless (zerop (mem-ref (+ rec #x20) :u32)) (return 0))))

(defun %bt-run (fn)
  (handler-case (funcall fn)
    (error () :error)))

(defun %bt-core-main ()
  "A secondary core's whole life: wait SAFE (so a region-0 collection never waits for
   an idle core), take the job, run it, publish the result, mark done."
  (let ((rec (%bt-rec (percpu-ref 16))))
    (%gc-write64 (+ rec #x68) 2)
    (loop
      (%gc-safe-enter)
      (%bt-wait-job rec)
      (%gc-safe-leave)
      (let ((fn (mem-ref (+ rec #x10) :u64)))
        (setf (mem-ref (+ rec #x20) :u32) 0)
        (setf (mem-ref (+ rec #x28) :u32) 1)         ; taken: FN is on this stack now
        (let ((v (%bt-run fn)))
          (setf (mem-ref (+ rec #x18) :u64) v)
          (mfence)
          (setf (mem-ref (+ rec #x08) :u32) 0))))))

;;; ------------------------------------------------------------------ the spawner side

(defun %bt-free-core ()
  (let ((r -1))
    (do ((c 1 (+ c 1))) ((or (>= c (%bt-ncores)) (>= r 0)))
      (let ((rec (%bt-rec c)))
        (when (and (zerop (mem-ref (+ rec #x08) :u32))
                   (not (zerop (mem-ref (+ rec #x58) :u32))))
          (setq r c))
        (when (zerop (mem-ref (+ rec #x30) :u32))
          (setq r c))))
    r))

(defun %make-native-thread (fn &rest args)
  "Run FN (no arguments) on a free core.  The core number (1..3), or -1 when every
   core is busy or unjoined, -3 when no region could be carved, -4 when the core
   did not start, -5 when it never took the job."
  (declare (ignore args))
  (%bt-init)
  (let ((c (%bt-free-core)))
    (if (< c 0)
        -1
        (let ((rec (%bt-rec c)))
          (if (and (zerop (mem-ref (+ rec #x30) :u32))
                   (or (zerop (%bt-carve c)) (zerop (%bt-core-boot c))))
              (if (zerop (mem-ref (+ #x10000F08 (* 8 c)) :u64)) -3 -4)
              (progn
                ;; Nothing below allocates until the core has taken FN, so region 0
                ;; cannot collect and move it while its only other reference is
                ;; this raw word.
                (setf (mem-ref (+ rec #x58) :u32) 0)
                (setf (mem-ref (+ rec #x28) :u32) 0)
                (setf (mem-ref (+ rec #x08) :u32) 1)
                (setf (mem-ref (+ rec #x10) :u64) fn)
                (mfence)
                (setf (mem-ref (+ rec #x20) :u32) 1)
                (let ((ok nil))
                  (dotimes (i 50000000)
                    (unless (zerop (mem-ref (+ rec #x28) :u32)) (setq ok t) (return nil)))
                  (if ok c -5))))))))

(defun %native-thread-alive-p (c)
  (if (zerop (mem-ref (+ (%bt-rec c) #x08) :u32)) 0 1))

(defun %join-native-thread (c budget)
  "0 once core C's job is done (the core is then free for the next one), 1 if BUDGET
   polls ran out first."
  (let ((r 1) (i 0))
    (loop
      (when (zerop (mem-ref (+ (%bt-rec c) #x08) :u32)) (setq r 0) (return nil))
      (when (>= i budget) (return nil))
      (setq i (+ i 1)))
    (when (zerop r) (setf (mem-ref (+ (%bt-rec c) #x58) :u32) 1))
    r))

(defun %bt-in-region-p (x c)
  "Whether X is an object in core C's region (either semispace)."
  (let ((w (%val->word x)) (rcb (%bt-rcb c)))
    (and (not (zerop (logand w 1)))
         (let ((size (%gc-meta-read (+ rcb #x10) 2))
               (f (%gc-meta-read rcb 2))
               (u (%gc-meta-read (+ rcb 8) 2)))
           (or (and (>= w f) (< w (+ f size)))
               (and (>= w u) (< w (+ u size))))))))

(defun %bt-copy-out (x c)
  "X, with every cons, string and simple vector that lives in core C's region copied
   into the caller's heap (the region is reused by the core's next job)."
  (cond ((not (%bt-in-region-p x c)) x)
        ((consp x) (cons (%bt-copy-out (car x) c) (%bt-copy-out (cdr x) c)))
        ((stringp x) (copy-seq x))
        ((simple-vector-p x)
         (let ((v (make-array (length x))))
           (dotimes (i (length x) v) (setf (svref v i) (%bt-copy-out (svref x i) c)))))
        ((vectorp x) (copy-seq x))
        (t x)))

(defun %thr-cpu ()
  (if (= (mem-ref #x10000FF8 :u32) 0) 0 (percpu-ref 16)))

;;; ------------------------------------------------------------------ public

(defun bt-spawn (fn)
  "Start FN (a function of no arguments) on a free core; the core number."
  (let ((c (%make-native-thread fn)))
    (if (< c 0) (error "bt-spawn: no core (~D)" c) c)))

(defun bt-join (c)
  "Wait for core C's job; its value, copied out of the core's region."
  (%join-native-thread c most-positive-fixnum)
  (%bt-copy-out (mem-ref (+ (%bt-rec c) #x18) :u64) c))

(defun bt-status ()
  "Per core: (busy started joined region-collections)."
  (loop for c from 0 below (%bt-ncores)
        collect (let ((rec (%bt-rec c)))
                  (list (mem-ref (+ rec #x08) :u32) (mem-ref (+ rec #x30) :u32)
                        (mem-ref (+ rec #x58) :u32)
                        (if (zerop c) 0 (%gc-meta-read (+ (%bt-rcb c) #x20) 2))))))

;;; ------------------------------------------------------------------ SMP mutex

(defun %mutex-trylock (cell) (if (zerop (xchg-mem cell 1)) 1 0))
(defun %mutex-lock (cell)
  (loop (when (zerop (xchg-mem cell 1)) (return 1))
        (loop (when (zerop (mem-ref cell :u32)) (return nil)))))
(defun %mutex-unlock (cell) (mfence) (xchg-mem cell 0) 0)
