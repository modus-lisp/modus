;;;; a64-pmu.lisp — the Cortex-A53 performance counters from bare-metal Modus.
;;;;
;;;;   (pmu-measure (lambda () (decode-all ivf)))          ; :summary preset
;;;;   (pmu-measure (lambda () (desk-tick d)) :stalls)
;;;;   (pmu-measure (lambda () ...) :inst-retired :l1i-refill :br-mis-pred)
;;;;   => (values RESULT COUNTS), and COUNTS printed as a table (cycles, IPC,
;;;;      each event and its rate per 1000 instructions)
;;;;   (pmu-start :inst-retired ...) ... (pmu-read)  ; the same, by hand
;;;;   (pmu-events)                                   ; the names
;;;;
;;;; Baked into the Pi board image (build-cl-repl-common.lisp, after fdt.lisp),
;;;; so it is there after every boot and in every core.  BARE METAL ONLY: Modus
;;;; runs at EL2 there.  A hosted (Linux, EL0) Modus traps on these registers;
;;;; there use perf_event_open (docs/reel-on-zero/zlinux/pstat.c).
;;;;
;;;; THE THING THAT TOOK A MONTH (docs/reel-on-zero/BOARD-RUNBOOK.md, PMU).  At
;;;; EL2 an A53 counter counts only with the NSH bit (27) set in its filter --
;;;; PMEVTYPERn_EL0 for an event counter, PMCCFILTR_EL0 for cycles; the reset
;;;; value counts nothing at EL2.  The September helpers set NSH and the cycle
;;;; counter worked, yet hardware events stayed 0 while software increments
;;;; counted.  2026-10-09: a full system-register dump from inside Modus matched
;;;; the 399-byte bare payload that DOES count (EL2 controls, debug/OS-lock
;;;; state, PMU block, CPUACTLR/CPUECTLR/L2CTLR/L2ECTLR/L2ACTLR; only MAIR, page
;;;; tables and vectors differ), and the payload's own sequence run as a stub
;;;; inside Modus counted exactly as the payload does.  So nothing in the
;;;; machine or Modus's environment blocks events: the old helpers'
;;;; programming did (PMSELR/PMXEVTYPER selection spread over several calls).
;;;; These stubs write PMEVTYPERn_EL0 / PMCNTENSET_EL0 DIRECTLY, in one call.
;;;;
;;;; HOW.  One 4 KB exec page holds three stubs (program at +0, read at +128,
;;;; stop at +256) and their data (+512).  Each stub starts with three words that
;;;; load the page's own address into x9 (the page sits below 4 GB on the Pi),
;;;; then the words below, assembled with `as' on the board host:
;;;;   program: PMCR=LC|C|P (stop, reset), clear enables/irqs/overflows,
;;;;            PMCCFILTR=NSH, PMEVTYPER0..5 <- [x9+512..552], PMCNTENSET <-
;;;;            [x9+560], PMCR=LC|C|P|E
;;;;   read:    PMCCNTR -> [x9+576], PMEVCNTR0..5 -> [x9+584..624],
;;;;            PMOVSSET -> [x9+632]
;;;;   stop:    PMCR=LC (E=0)
;;;; The A53's event counters are 32 bits (cycles: 64 with LC); a count that
;;;; wrapped is reported, not hidden -- keep a measurement under ~4e9 events.
;;;;
;;;; BAKED SOURCE: a DEFVAR's initform never runs in the image, so the page is
;;;; made on first use (BOUNDP), and the tables are functions.

(defvar *pmu-page*)        ; the stub page, made by %PMU-PAGE on first use
(defvar *pmu-current*)     ; the event names PMU-START programmed, counter order

(defun %pmu-program-words ()
  (list #xd28008c1 #xd51b9c01 #xd5033fdf #xb2407fe1 #xd51b9c41 #xd5189e41 #xd51b9c61
        #xd2a10001 #xd51befe1 #xf9410121 #xd51bec01 #xf9410521 #xd51bec21 #xf9410921
        #xd51bec41 #xf9410d21 #xd51bec61 #xf9411121 #xd51bec81 #xf9411521 #xd51beca1
        #xf9411921 #xd51b9c21 #xd5033fdf #xd28008e1 #xd51b9c01 #xd5033fdf #xd2800000
        #xd65f03c0))

(defun %pmu-read-words ()
  (list #xd5033fdf #xd53b9d00 #xf9012120 #xd53be800 #xf9012520 #xd53be820 #xf9012920
        #xd53be840 #xf9012d20 #xd53be860 #xf9013120 #xd53be880 #xf9013520 #xd53be8a0
        #xf9013920 #xd53b9e60 #xf9013d20 #xd2800000 #xd65f03c0))

(defun %pmu-stop-words ()
  (list #xd2800801 #xd51b9c01 #xd5033fdf #xd2800000 #xd65f03c0))

(defun pmu-events ()
  "The A53 events by name: (name . number).  Architectural events plus the A53's
   implementation-defined stall events (Cortex-A53 TRM, PMU events 0xC0-0xE8)."
  '((:sw-incr . #x00) (:l1i-refill . #x01) (:l1i-tlb-refill . #x02)
    (:l1d-refill . #x03) (:l1d-access . #x04) (:l1d-tlb-refill . #x05)
    (:ld-retired . #x06) (:st-retired . #x07) (:inst-retired . #x08)
    (:exc-taken . #x09) (:exc-return . #x0a) (:pc-write-retired . #x0c)
    (:br-immed-retired . #x0d) (:unaligned-ldst-retired . #x0f)
    (:br-mis-pred . #x10) (:cpu-cycles . #x11) (:br-pred . #x12)
    (:mem-access . #x13) (:l1i-access . #x14) (:l1d-wb . #x15)
    (:l2d-access . #x16) (:l2d-refill . #x17) (:l2d-wb . #x18)
    (:bus-access . #x19) (:bus-cycles . #x1d)
    (:iq-empty . #xe0) (:iq-empty-icmiss . #xe1) (:iq-empty-utlb . #xe2)
    (:iq-empty-predec . #xe3) (:ilock-other . #xe4) (:ilock-load . #xe5)
    (:ilock-store . #xe6) (:lsu-busy . #xe7) (:sb-full . #xe8)))

(defun pmu-preset (name)
  "Six-event sets (the A53 has six counters; cycles are counted besides).
   :SUMMARY  where instructions and misses go
   :STALLS   why the in-order pipeline waits (IQ empty = front end; interlocks
             and LSU = back end)
   :MEMORY   the data side"
  (case name
    (:summary (list :inst-retired :l1i-refill :l1d-refill :l2d-refill :br-mis-pred :iq-empty-icmiss))
    (:stalls (list :inst-retired :iq-empty :iq-empty-icmiss :ilock-other :ilock-load :lsu-busy))
    (:memory (list :inst-retired :l1d-access :l1d-refill :l1d-tlb-refill :l2d-refill :bus-access))
    (t nil)))

(defun %pmu-event-number (e)
  (cond ((integerp e) e)
        ((assoc e (pmu-events)) (cdr (assoc e (pmu-events))))
        (t (error "pmu: unknown event ~s (see (pmu-events))" e))))

(defun %pmu-put64 (a v)
  ;; two :u32 halves: a :u64 store doubles a fixnum (see %core-copy-words)
  (setf (mem-ref a :u32) (logand v #xffffffff))
  (setf (mem-ref (+ a 4) :u32) (logand (ash v -32) #xffffffff)))

(defun %pmu-get64 (a)
  (logior (mem-ref a :u32) (ash (mem-ref (+ a 4) :u32) 32)))

(defun %pmu-install (p off words)
  ;; movz/movk x9 <- P, then WORDS, at P+OFF
  (let ((i 0))
    (dolist (w (append (list (logior #xd2800009 (ash (logand p #xffff) 5))
                             (logior #xf2a00009 (ash (logand (ash p -16) #xffff) 5))
                             #xf2c00009)
                       words))
      (setf (mem-ref (+ p off i) :u32) w)
      (setq i (+ i 4)))))

(defun %pmu-page ()
  (unless (and (boundp '*pmu-page*) (integerp *pmu-page*))
    (let ((p (%mmap-exec-page 4096)))
      (%pmu-install p 0 (%pmu-program-words))
      (%pmu-install p 128 (%pmu-read-words))
      (%pmu-install p 256 (%pmu-stop-words))
      (%jit-icache-flush p 512)
      (setq *pmu-page* p)))
  *pmu-page*)

(defun pmu-start (&rest events)
  "Reset and start the cycle counter and up to six EVENTS (names from
   (pmu-events), numbers, or one preset keyword such as :SUMMARY)."
  (when (and events (null (cdr events)) (pmu-preset (car events)))
    (setq events (pmu-preset (car events))))
  (when (> (length events) 6)
    (error "pmu: the A53 has six event counters, not ~d" (length events)))
  (let ((p (%pmu-page)) (mask #x80000000) (i 0))
    (dotimes (k 6)
      (%pmu-put64 (+ p 512 (* 8 k)) 0))
    (dolist (e events)
      (%pmu-put64 (+ p 512 (* 8 i)) (logior (%pmu-event-number e) #x08000000)) ; NSH: count at EL2
      (setq mask (logior mask (ash 1 i)))
      (setq i (+ i 1)))
    (%pmu-put64 (+ p 560) mask)
    (setq *pmu-current* events)
    (%jit-call p)
    events))

(defun pmu-read ()
  "The counts since PMU-START, as a plist: (:CYCLES n EVENT n ...), plus
   :OVERFLOWED (the events whose 32-bit counter wrapped) when any did."
  (let ((p (%pmu-page)) (out nil) (i 0) (ovf nil))
    (%jit-call (+ p 128))
    (let ((ovs (%pmu-get64 (+ p 632))))
      (dolist (e (if (boundp '*pmu-current*) *pmu-current* nil))
        (setq out (append out (list e (%pmu-get64 (+ p 584 (* 8 i))))))
        (when (logbitp i ovs) (setq ovf (append ovf (list e))))
        (setq i (+ i 1)))
      (append (list :cycles (%pmu-get64 (+ p 576))) out
              (if ovf (list :overflowed ovf) nil)))))

(defun pmu-stop ()
  (%jit-call (+ (%pmu-page) 256))
  nil)

(defun pmu-report (counts &optional (stream t))
  "Print COUNTS (from PMU-READ): cycles, IPC, and each event with its rate per
   1000 instructions when :INST-RETIRED was counted."
  (let ((cyc (getf counts :cycles)) (inst (getf counts :inst-retired)))
    (format stream "~&  cycles ~15:d~@[  IPC ~,2f~]~%" cyc
            (and inst cyc (> cyc 0) (/ inst (float cyc))))
    (loop for (k v) on counts by #'cddr
          unless (member k '(:cycles :overflowed))
            do (format stream "  ~22a ~15:d~@[  ~,2f/k-inst~]~@[  ~,1f% of cycles~]~%"
                       (string-downcase (symbol-name k)) v
                       (and inst (> inst 0) (not (eq k :inst-retired)) (/ (* 1000.0 v) inst))
                       (and cyc (> cyc 0) (member k '(:iq-empty :iq-empty-icmiss :iq-empty-utlb
                                                      :iq-empty-predec :ilock-other :ilock-load
                                                      :ilock-store :lsu-busy :sb-full))
                            (/ (* 100.0 v) cyc))))
    (when (getf counts :overflowed)
      (format stream "  !! wrapped (32-bit): ~{~a~^ ~}~%" (getf counts :overflowed)))
    counts))

(defun pmu-measure (thunk &rest events)
  "Run THUNK with the counters on; print the table and return (values RESULT
   COUNTS).  EVENTS as for PMU-START; none means :SUMMARY."
  (apply #'pmu-start (or events (list :summary)))
  (let* ((r (funcall thunk))
         (counts (pmu-read)))
    (pmu-stop)
    (pmu-report counts)
    (values r counts)))
