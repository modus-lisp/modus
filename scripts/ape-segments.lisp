;;;; ape-segments.lisp -- append named segments to an executable, with the trailer
;;;; lib/cli-toplevel.lisp reads (APE-SEGMENTS / APE-SEGMENT).
;;;;
;;;; Loaded by make-ape.lisp (modus.com), and by kiln's bundle (kiln/boot/bundle.lisp),
;;;; which appends kiln and cairn to modus.exe for --kiln.  An executable with segments
;;;; still runs as it did: Windows ignores data after a PE's last section, and the ELF
;;;; loader reads only what the program headers name.  Runs on SBCL and on modus.

(defun ape-trailer (out segments &optional placed)
  "OUT (an octet vector) with SEGMENTS ((NAME . OCTETS) ...) appended, 4 KB aligned, and
   the directory and trailer lib/cli-toplevel.lisp reads (APE-SEGMENTS): the text of
   ((NAME OFFSET LENGTH) ...), then \"MODUSAPE\", its offset and length (8 bytes each,
   little-endian) and 8 zero bytes."
  (let* ((at (length out)) (dir (reverse placed)) (parts nil))
    (dolist (seg segments)
      (setq at (* 4096 (ceiling at 4096)))
      (push (list (car seg) at (length (cdr seg))) dir)
      (push (cons at (cdr seg)) parts)
      (incf at (length (cdr seg))))
    (let* ((text (let ((*print-pretty* nil) (*print-base* 10))
                   (prin1-to-string (reverse dir))))
           (dir-at at)
           (total (+ dir-at (length text) 32))
           (new (make-array total :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace new out)
      (dolist (p parts) (replace new (cdr p) :start1 (car p)))
      (dotimes (i (length text)) (setf (aref new (+ dir-at i)) (char-code (char text i))))
      (let ((tr (+ dir-at (length text))))
        (dotimes (i 8) (setf (aref new (+ tr i)) (char-code (char "MODUSAPE" i))))
        (dotimes (k 8) (setf (aref new (+ tr 8 k)) (ldb (byte 8 (* 8 k)) dir-at)))
        (dotimes (k 8) (setf (aref new (+ tr 16 k)) (ldb (byte 8 (* 8 k)) (length text)))))
      new)))
