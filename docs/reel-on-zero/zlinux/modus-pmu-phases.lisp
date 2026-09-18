;;; modus-pmu-phases.lisp — per-phase A53 PMU counters from inside hosted Modus (Linux on the Zero).
;;; Same 13 phase wrappers as docs/reel-on-zero/serial-prof.py, but each wrapper accumulates the DELTA of
;;; one perf counter set (bundle 7: cycles, inst, L1I refill, L1D refill, br mispred, L2 refill) per phase.
;;; Wrapper cost: 2 grouped read(2) per call (all 6 counters in one read); residue is the most-called phase.
(unless (boundp '*dir*) (setq *dir* "/reel/"))        ; --eval '(setq *dir* "/home/modus/reelbench/src/")' on the Pi 5
(unless (boundp '*clip*) (setq *clip* "/reel/cam.ivf"))
(unless (boundp '*pmu-lib*) (setq *pmu-lib* "/reel/modus-pmu.lisp"))
(load *pmu-lib*)
(dolist (f '("packages" "decode/tables" "decode/bool" "decode/transform" "decode/intra" "decode/loopfilter" "decode/inter-tables" "decode/inter"))
  (load (concatenate 'string *dir* f ".lisp")))
(dolist (f (quote ("decode/intra-neon" "decode/inter-neon" "decode/loopfilter-neon"))) (load (concatenate (quote string) *dir* f ".lisp")))
(defvar *events* (list #x11 #x08 #x01 #x03 #x10 #x17))
(defvar *nev* 6)
(defvar *phases* nil)                      ; list of (name . u64-accumulator-vector)
(unless (boundp '*no-pmu*) (setq *no-pmu* nil))   ; --eval '(setq *no-pmu* t)': wrappers only, no counters (bisect knob)
(unless (boundp '*snap-off*) (setq *snap-off* nil))   ; --eval '(setq *snap-off* t)': wrappers installed, snapshots skipped (bisect knob)
(defun %pmu-snap (into)
  (declare (type (simple-array (signed-byte 32) (*)) into) (optimize (speed 3) (safety 0)))
  (when (and *pmu-fds* (not *snap-off*))
    (syscall3 0 (car *pmu-fds*) *io-buf-addr* 56)                       ; one grouped read: {nr, v0..v5}
    (dotimes (i 6) (declare (type fixnum i)) (setf (aref into i) (%u64-at (+ *io-buf-addr* 8 (* 8 i)))))))   ; one grouped read(2) per snapshot
;; (signed-byte 32) is on the compiler's inline whitelist: stored as plain tagged words (NO 32-bit truncation — cycle
;; totals exceed 2^32), but a declared local gets the 5-instruction inline aref instead of the ~230-instruction generic
;; path through an undeclared special (measured on the Pi 5: a 6-element accumulate loop 5545 -> 381 instructions).
(defun %counter-vec () (make-array 6 :element-type '(signed-byte 32) :initial-element 0))
(defvar *snap-a* (%counter-vec))
(defvar *snap-b* (%counter-vec))
(defvar *depth* 0)
(defmacro defphase (name fn args)
  "Wrap FN: snapshot counters on entry into a per-phase preallocated array, add deltas on exit. No per-call allocation
   (the Zero has 415 MB; a GC that touches the second semispace gets Modus OOM-killed). Nested phases are inclusive.
   MODUS BUG (2026-09-18, both arches): DEFUNs inside ONE top-level form are installed before the form's earlier
   subforms run, so a capture of (symbol-function 'fn) in the same PROGN as (defun fn ...) captures the NEW fn and
   the wrapper recurses until the stack overflows (SIGSEGV in %GV-CELL). So the capture is a SEPARATE top-level
   eval (%capture-original) and this macro expands to the redefinition only.
   MODUS PERF (2026-09-18): (funcall *captured-fn* ...) of a runtime-native function VALUE costs ~20k instructions
   per call (measured: 23 M inst/frame over 1000 wrapped calls/frame); calling the original under a fresh NAME
   (ORIG-x, installed with (setf symbol-function)) lets the JIT relocate a direct call. Do that."
  (let ((o (intern (format nil "ORIG-~a" name) (symbol-package fn))) (acc (intern (format nil "*A-~a*" name))) (snap (intern (format nil "*S-~a*" name))))
    `(progn
       (defvar ,acc (%counter-vec))
       (defvar ,snap (%counter-vec))
       (push (cons ',name ,acc) *phases*)
       (defun ,fn ,args
         (let ((acc ,acc) (snap ,snap) (sb *snap-b*))
           (declare (type (simple-array (signed-byte 32) (*)) acc snap sb) (optimize (speed 3) (safety 0)))
           (%pmu-snap snap)
           (,(if (and (boundp '*use-prog1*) *use-prog1* (not (eq name 'residue))) 'prog1 'multiple-value-prog1) (,o ,@args)
             (%pmu-snap sb)
             (dotimes (i 6) (declare (type fixnum i)) (setf (aref acc i) (+ (aref acc i) (- (aref sb i) (aref snap i)))))))))))
(defun %capture-original (name fn)
  (setf (symbol-function (intern (format nil "ORIG-~a" name) (symbol-package fn))) (symbol-function fn)))
(unless (boundp '*phase-set*) (setq *phase-set* :all))          ; --eval '(defvar *phase-set* (quote (loopf)))' before --load to wrap a subset
(unless (boundp '*wrap-after-eager*) (setq *wrap-after-eager* nil))    ; --eval '(defvar *wrap-after-eager* t)': install wrappers AFTER jit-eager (serial-prof.py order)
(defun install-phases ()
  (when (or (eq *phase-set* :all) (member 'residue *phase-set*)) (%capture-original 'residue 'reel.decode::decode-residue) (eval '(defphase residue reel.decode::decode-residue (d bd mbx skip has-y2 dq))))
  (when (or (eq *phase-set* :all) (member 'luma16 *phase-set*)) (%capture-original 'luma16 'reel.decode::reconstruct-luma16) (eval '(defphase luma16 reel.decode::reconstruct-luma16 (d mx my ymode ha hl))))
  (when (or (eq *phase-set* :all) (member 'bpred *phase-set*)) (%capture-original 'bpred 'reel.decode::reconstruct-bpred) (eval '(defphase bpred reel.decode::reconstruct-bpred (d mx my))))
  (when (or (eq *phase-set* :all) (member 'chroma *phase-set*)) (%capture-original 'chroma 'reel.decode::reconstruct-chroma) (eval '(defphase chroma reel.decode::reconstruct-chroma (d mbx mby uvmode ha hl))))
  (when (or (eq *phase-set* :all) (member 'interpred *phase-set*)) (%capture-original 'interpred 'reel.decode::predict-inter-mb) (eval '(defphase interpred reel.decode::predict-inter-mb (vd mi mbx mby mode))))
  (when (or (eq *phase-set* :all) (member 'interres *phase-set*)) (%capture-original 'interres 'reel.decode::add-inter-residual) (eval '(defphase interres reel.decode::add-inter-residual (d mx my cx cy))))
  (when (or (eq *phase-set* :all) (member 'mbmodes *phase-set*)) (%capture-original 'mbmodes 'reel.decode::read-mb-modes) (eval '(defphase mbmodes reel.decode::read-mb-modes (d part0 mbx))))
  (when (or (eq *phase-set* :all) (member 'intermodes *phase-set*)) (%capture-original 'intermodes 'reel.decode::read-inter-modes) (eval '(defphase intermodes reel.decode::read-inter-modes (vd part0 mi mbx mby))))
  (when (or (eq *phase-set* :all) (member 'intramodes *phase-set*)) (%capture-original 'intramodes 'reel.decode::read-intra-modes-inter-frame) (eval '(defphase intramodes reel.decode::read-intra-modes-inter-frame (vd part0))))
  (when (or (eq *phase-set* :all) (member 'lfparams *phase-set*)) (%capture-original 'lfparams 'reel.decode::mb-filter-params) (eval '(defphase lfparams reel.decode::mb-filter-params (vd d mi key))))
  (when (or (eq *phase-set* :all) (member 'loopf *phase-set*)) (%capture-original 'loopf 'reel.decode::loop-filter-frame) (eval '(defphase loopf reel.decode::loop-filter-frame (vd key))))
  (when (or (eq *phase-set* :all) (member 'refcopy *phase-set*)) (%capture-original 'refcopy 'reel.decode::plane->rframe-plane) (eval '(defphase refcopy reel.decode::plane->rframe-plane (pl dst stride border w h))))
  (when (or (eq *phase-set* :all) (member 'mbloop *phase-set*)) (%capture-original 'mbloop 'reel.decode::decode-macroblocks) (eval '(defphase mbloop reel.decode::decode-macroblocks (vd part0 tokens nparts key))))
  (length *phases*))
(unless *wrap-after-eager* (install-phases))
(format t "installed=~a~%" (length *phases*)) (finish-output)
(format t "eager=~a~%" (jit-eager)) (finish-output)
(when *wrap-after-eager* (install-phases))
(format t "phases=~a set=~a after-eager=~a~%" (length *phases*) *phase-set* *wrap-after-eager*) (finish-output)
(format t "native? residue=~a orig-residue=~a lfparams=~a orig-lfparams=~a snap=~a~%" (%jit-fn-native-p "REEL.DECODE::DECODE-RESIDUE") (%jit-fn-native-p "REEL.DECODE::ORIG-RESIDUE") (%jit-fn-native-p "REEL.DECODE::MB-FILTER-PARAMS") (%jit-fn-native-p "REEL.DECODE::ORIG-LFPARAMS") (%jit-fn-native-p "%PMU-SNAP")) (finish-output)
(defun rfb (path) (with-open-file (s path :element-type '(unsigned-byte 8))
  (let* ((n (file-length s)) (v (make-array n :element-type '(unsigned-byte 8)))) (dotimes (i n) (setf (aref v i) (read-byte s))) v)))
(defun le32 (v o) (logior (aref v o) (ash (aref v (+ o 1)) 8) (ash (aref v (+ o 2)) 16) (ash (aref v (+ o 3)) 24)))
(defun decode-all (ivf) (let ((vd (reel:make-decoder)) (off 32) (n 0)) (loop (when (>= (+ off 12) (length ivf)) (return)) (let* ((sz (le32 ivf off)) (s (+ off 12)) (e (+ s sz))) (reel:decode-frame vd ivf :start s :end e) (setq off e) (incf n))) n))
(let ((ivf (rfb *clip*)))
  (unless *no-pmu* (pmu-start *events*))
  (pmu-snap *snap-a*)
  (let ((n (decode-all ivf)))
    (pmu-snap *snap-b*)
    (unless *no-pmu* (pmu-stop))
    (format t "PMU-PHASES frames=~a total cycles/frame=~a inst/frame=~a~%" n
            (round (- (aref *snap-b* 0) (aref *snap-a* 0)) n) (round (- (aref *snap-b* 1) (aref *snap-a* 1)) n))
    (format t "~12a ~9a ~9a ~6a ~7a ~7a ~7a ~7a~%" "phase" "kcyc/fr" "kinst/fr" "IPC" "L1I/ki" "L1D/ki" "brm/ki" "L2/ki")
    (dolist (p (reverse *phases*))
      (let ((a (cdr p)))
        (format t "~12a ~9a ~9a ~6,2f ~7,2f ~7,2f ~7,2f ~7,2f~%" (car p)
                (round (aref a 0) (* 1000 n)) (round (aref a 1) (* 1000 n))
                (/ (aref a 1) (max 1 (aref a 0)))
                (/ (* 1000.0 (aref a 2)) (max 1 (aref a 1))) (/ (* 1000.0 (aref a 3)) (max 1 (aref a 1)))
                (/ (* 1000.0 (aref a 4)) (max 1 (aref a 1))) (/ (* 1000.0 (aref a 5)) (max 1 (aref a 1))))))))
(format t "PMU-PHASES DONE~%")
