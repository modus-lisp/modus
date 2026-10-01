;;;; save-and-die.lisp — phase 1 of run-save-and-die.sh: JIT two functions,
;;;; keep some data, snapshot.  The core path is the first --eval'd *CORE*.
(defun sad-mul (x) (* x 7))
(defun sad-loop (n) (let ((s 0)) (dotimes (i n) (setq s (+ s (sad-mul i)))) s))
(defvar *sad-kept* (list 1 "two" :three (make-hash-table)))
(setf (gethash :k (fourth *sad-kept*)) 42)
;; A mutex carves the hosted actor band off region 0 and takes a sync cell: the
;; core must carry the band (the extra range), the cell (the exec arena), and
;; region 0's shrunk size.
(defvar *sad-mutex* (sb-thread:make-mutex :name "sad"))
(sb-thread:with-mutex (*sad-mutex*) nil)
(sad-loop 1000)
(jit-eager)
;; Three forced collections: at least one starts from an ODD count, which x64
;; stores raw -- %gc-force's counter read used to fault there.
(%gc-force) (%gc-force) (%gc-force)
(unless (%jit-fn-native-p "COMMON-LISP-USER::SAD-LOOP")
  (format t "SAD: FAIL sad-loop not native before save~%") (sys-exit 3))
(save-and-die *core*)
