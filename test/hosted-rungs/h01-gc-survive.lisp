;;;; h01-gc-survive.lisp -- expect 42
;;;;
;;; HOSTED ONLY: a hosted collector must move live data and leave it intact.
;;; (The bare targets have no collector, so this is not an arch-ladder rung.)
;;;
;;; Live across the churn, each rooted ONLY by a local in PROBE's frame:
;;;   a 1000-cons chain            -- cons copying + stack roots
;;;   a 3-slot array holding 35    -- headered object copying
;;;   a 16384-slot string, slot 0 = 7 -- a large object (the ADDI-wrapped case)
;;; The churn conses 64M throwaway cells, 1 GB: several collections at 4- and
;;; 8-byte words alike.  Then:  (sum 1..1000 - 500500) + 35 + 7 = 42.
;;;
;;; Run with:  scripts/hosted-ladder.py riscv32 test/hosted-rungs/h01-gc-survive.lisp

(defun build (n acc) (if (= n 0) acc (build (- n 1) (cons n acc))))
(defun sum-list (l acc) (if (consp l) (sum-list (cdr l) (+ acc (car l))) acc))
(defun churn (n)
  (let ((i 0) (junk nil))
    (loop (when (>= i n) (return junk))
      (setq junk (cons i junk))
      (when (= 0 (logand i 1023)) (setq junk nil))
      (setq i (+ i 1)))))

(defun probe ()
  (let ((chain (build 1000 nil))
        (v (make-array 3))
        (s (%make-string-array 16384)))
    (%prim-aset v 1 35)
    (%prim-aset s 0 7)
    (churn 67108864)
    (+ (- (sum-list chain 0) 500500) (%prim-aref v 1) (%prim-aref s 0))))
