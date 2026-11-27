;;;; native-bulk-u32.lisp -- REPLACE and FILL on packed (unsigned-byte 32) vectors as
;;;; BULK-MOVEs of bytes.  Kind 1 addresses bytes as %U8-REF does, so this is right only
;;;; where a packed u32 vector's payload starts where a byte vector's does: the 64-bit
;;;; layouts (both at the tagged word + 7).  aarch64 and x64 load it; i386, whose byte
;;;; vectors start at word - 5, keeps prelude.lisp's lane loops.

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

;;; Packed double-float vectors (#x18), on the same footing: 8-byte lanes at data + 8I, the
;;; byte vector's layout on the 64-bit targets.  Byte moves carry raw IEEE bits -- a WORD
;;; move (kind 0) would run each lane through the shared-store guard as a pointer.

(defun %bulk-copy-f64 (dst dstart src sstart n)
  (when (> n 0) (%bulk-move dst (* 8 dstart) src (* 8 sstart) (* 8 n) 1))
  dst)

(defun %fill-f64 (seq start end item)
  ;; the first lane, then double the filled span, as %FILL-U32
  (when (< start end)
    (%f64-aset-rt seq start item)
    (let ((done 1) (n (- end start)))
      (loop (when (>= done n) (return nil))
        (let ((k (min done (- n done))))
          (%bulk-move seq (* 8 (+ start done)) seq (* 8 start) (* 8 k) 1)
          (setq done (+ done k))))))
  seq)
