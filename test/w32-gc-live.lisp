;;;; test/w32-gc-live.lisp -- the W32 COLLECTOR RUNG (docs/w32-spec.md 8), expect 15
;;;;
;;; A raw W32 word whose bits ARE the tagged address of a live from-space
;;; object (a cons), kept in a (declare (w32-raw ...)) variable across a forced
;;; collection (%GC-COLLECT-HERE).  The compiler must split it around that GC
;;; point (compiler.lisp %W32-RAW-SCOPE-REWRITE): the word then reads back
;;; unchanged.  Built with MODUS_W32_UNSAFE_LIVE=1 the split is skipped, the
;;; collector takes the word for a pointer and forwards it, and this answers
;;; 11 -- bit 2 missing -- deterministically: the value is a real object's
;;; address, so it passes every test the collector makes of a pointer.
;;;
;;; The answer is a bitmask, so a wrong one says which check failed:
;;;   1  a collection ran (%GC-EPOCH changed; %GC-COUNT is not a fixnum on i386)
;;;   2  the cons moved (its tagged address after != before): the test means
;;;      something only if the collector really moved what the word points at
;;;   4  the raw word reads back as it was written (THE rung)
;;;   8  a second raw word, #xFFFFFFFE (fixnum-tagged, so harmless to the
;;;      collector even unsplit), live across the same collection, too:
;;;      the split's halves and join round-trip a word no fixnum holds
;;;
;;; It needs a collector, so it runs in the CLI images (the bare rung images
;;; have none to force): built in by the BUILD-TIME compiler (AOT), on each
;;; W32-capable CLI --
;;;   MODUS_EXTRA_SOURCE=test/w32-gc-live.lisp sbcl --script mvm/build-riscv32-cli.lisp
;;;   qemu-riscv32-static IMAGE --eval '(modus-int::w32-gc-live)' --quit
;;; test/run-w32-gc-live.sh builds riscv32, arm32 and i386 both ways and
;;; checks 15 without the knob and 11 with it.

(defun w32-gc-live-probe ()
  (let ((x (cons 41 1)) (w 0) (m 0)
        (c0 0) (c1 0) (h0 0) (l0 0) (h 0) (l 0) (xh 0) (xl 0) (mh 0) (ml 0))
    (declare (w32-raw w m))
    (setq c0 (%gc-epoch))
    ;; W = X's slot, raw: the cons's tagged address
    (setq w (%w32 (w32-raw (w32-ref x))))
    (setq m (%w32 (w32-raw (w32-li #xFFFFFFFE))))
    (setq h0 (%w32 (w32-hi16 (w32-ref w))))
    (setq l0 (%w32 (w32-lo16 (w32-ref w))))
    ;; the GC point, with W and M live across it
    (%gc-collect-here)
    (setq c1 (%gc-epoch))
    (setq h (%w32 (w32-hi16 (w32-ref w))))
    (setq l (%w32 (w32-lo16 (w32-ref w))))
    (setq mh (%w32 (w32-hi16 (w32-ref m))))
    (setq ml (%w32 (w32-lo16 (w32-ref m))))
    ;; where the cons is now
    (setq xh (%w32 (w32-hi16 (w32-ref x))))
    (setq xl (%w32 (w32-lo16 (w32-ref x))))
    (+ (if (/= c1 c0) 1 0)
       (if (and (= (car x) 41) (not (and (= xh h0) (= xl l0)))) 2 0)
       (if (and (= h h0) (= l l0)) 4 0)
       (if (and (= mh 65535) (= ml 65534)) 8 0))))

(defun w32-gc-live ()
  (let ((r (w32-gc-live-probe)))
    (format t "~&w32-gc-live ~D (want 15)~%" r)
    r))
