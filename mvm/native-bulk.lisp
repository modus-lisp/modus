;;;; native-bulk.lisp — REPLACE and FILL on the BULK-MOVE / BULK-FILL instructions.
;;;;
;;;; REPLACE and FILL bottom out in four small loops -- %BULK-COPY, %BULK-COPY-U8,
;;;; %FILL-WORDS, %FILL-U8 (prelude.lisp) -- each a Lisp loop of one element
;;;; store.  On aarch64 and x64 those are one instruction each instead (ldp/stp
;;;; blocks, rep movsb / stosq; mvm.lisp +op-bulk-move+ / +op-bulk-fill+), and
;;;; this file replaces the four, last-defun-wins.  Measured on a Pi Zero 2 W
;;;; before: 33 ns a word for FILL, 55 for REPLACE -- most of a glass window
;;;; drag.  Only those two translators implement the opcodes, so only their
;;;; builds load this (build-cli-common *CLI-NATIVE-BULK-SOURCE*, and
;;;; build-ansi-common for the gate runners); everyone else keeps the loops.
;;;; The callers have already checked kinds and bounds.

(defun %bulk-copy (dst dstart src sstart n)
  (when (> n 0) (%bulk-move dst dstart src sstart n 0))
  dst)

(defun %bulk-copy-u8 (dst dstart src sstart n)
  (when (> n 0) (%bulk-move dst dstart src sstart n 1))
  dst)

(defun %bulk-copy-u32 (dst dstart src sstart n)
  ;; packed lanes are 4 bytes at data + 4I: a byte move of the scaled range
  (when (> n 0) (%bulk-move dst (* 4 dstart) src (* 4 sstart) (* 4 n) 1))
  dst)

(defun %fill-u32 (seq start end item)
  ;; BULK-FILL repeats a byte or a word, not a lane: store the first lane, then double the
  ;; filled span with forward moves of what is already there (log2 N moves, each a memcpy)
  (when (< start end)
    (%c32-set seq start item)
    (let ((done 1) (n (- end start)))
      (loop (when (>= done n) (return nil))
        (let ((k (min done (- n done))))
          (%bulk-move seq (* 4 (+ start done)) seq (* 4 start) (* 4 k) 1)
          (setq done (+ done k))))))
  seq)

(defun %fill-words (seq start end item)
  (when (< start end) (%bulk-fill seq start (- end start) item 0))
  seq)

(defun %fill-u8 (seq start end item)
  (when (< start end) (%bulk-fill seq start (- end start) item 1))
  seq)
