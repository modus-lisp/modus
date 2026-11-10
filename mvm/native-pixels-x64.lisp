;;;; native-pixels-x64.lisp -- socket output on BULK-MOVE kinds 2 and 3 (x86-64 only).
;;;;
;;;; cl-fileio's %FS-WRITE-PIXEL-ROWS stores one pixel per Lisp loop iteration: about 60
;;;; instructions a pixel, 1.1 ms for a 640x480 frame against SBCL's 0.4 ms for the same
;;;; loop.  Here each row segment is one BULK-MOVE kind 2, a native copy that clears the
;;;; top byte.  Only the x86-64 translator implements kind 2, so only that build loads this
;;;; file (build-cli-common *CLI-NATIVE-BULK-SOURCE*); it replaces the loop, last-defun-wins.
;;;; The blob reads this in MODUS-INT, and the stream helpers are MODUS.RUNTIME's: qualified.

(defun modus.runtime::%fs-write-pixel-rows (stream px fw x y w h)
  "Write the rectangle (X,Y,W,H) of PX (an (unsigned-byte 32) array, row stride FW) to the
   buffered SOCKET output STREAM as 4-byte little-endian pixels, B G R 0.  Each row goes
   into the per-CPU socket page in as few BULK-MOVEs as the page boundary allows.  Returns T
   when it wrote the rectangle; NIL, having written nothing, unless STREAM is a buffered
   socket output."
  (declare (type (simple-array (unsigned-byte 32) (*)) px) (fixnum fw x y w h)
           (optimize (speed 3) (safety 0)))
  (let ((fd (modus.runtime::%fs-fd stream)))
    (unless (modus.runtime::%fs-obuf-p stream fd) (return-from modus.runtime::%fs-write-pixel-rows nil))
    (modus.runtime::%fs-oflush stream)
    (let* ((page (modus-int::%sock-io-buf)) (cap (modus-int::%sock-io-cap)) (n 0) (total 0))
      (declare (fixnum page cap n total))
      (loop for yy fixnum from y below (+ y h) do
        (let ((src (+ (* yy fw) x)) (left w))
          (declare (fixnum src left))
          (loop while (> left 0) do
            (let ((k (min left (ash (- cap n) -2))))
              (declare (fixnum k))
              (%bulk-move page n px src k 2)
              (setq n (+ n (* 4 k)) src (+ src k) left (- left k))
              (when (= n cap)
                (modus.runtime::%fs-page-flush fd page cap)
                (setq total (+ total cap) n 0))))))
      (when (> n 0)
        (modus.runtime::%fs-page-flush fd page n)
        (setq total (+ total n)))
      (modus.runtime::%fs-set-pos stream (+ (modus.runtime::%fs-pos stream) total))
      t)))

(defun modus.runtime::%fs-page-copy (seq i n page)
  "Copy SEQ[I, I+N) (a simple (unsigned-byte 8) array) into the raw I/O page at PAGE: one
   BULK-MOVE kind 3 (rep movsb).  The Lisp loop it replaces was 213 us for a 99 KB ZRLE frame."
  (declare (type (simple-array (unsigned-byte 8) (*)) seq) (fixnum i n page))
  (when (> n 0) (%bulk-move page 0 seq i n 3))
  nil)

(defun modus.runtime::%u32-rect-uniform-p (px fw x y w h c)
  "T when every pixel of the rectangle (X,Y,W,H) of PX (u32 lanes, row stride FW) equals the
   u32 C: one KERNEL kind 0 per row."
  (declare (type (simple-array (unsigned-byte 32) (*)) px) (fixnum fw x y w h))
  (let ((yy y) (end (+ y h)))
    (declare (fixnum yy end))
    (loop
      (when (>= yy end) (return t))
      (unless (= (%kernel px (+ (* yy fw) x) c w 0) w) (return nil))
      (setq yy (+ yy 1)))))

(defun modus.runtime::%adler32-update (data start end a b)
  "Adler-32's A and B over the octets DATA[START,END): one KERNEL kind 1."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (fixnum start end))
  (if (>= start end)
      (values a b)
      (let ((ab (%kernel data start (logior (ash (mod b 65521) 16) (mod a 65521)) (- end start) 1)))
        (values (logand ab #xFFFF) (ash ab -16)))))

(defun modus.runtime::%u32-to-cpixels (buf i px si n)
  "The N pixels PX[SI..] into BUF from I as 3-byte CPIXELs: one BULK-MOVE kind 4."
  (declare (fixnum i si n))
  (when (> n 0) (%bulk-move buf i px si n 4))
  (+ i (* 3 n)))

(defun modus.runtime::%fixnum-vector-rebase (vec off)
  "Each fixnum X of VEC := X - OFF when X >= OFF, else -1: one KERNEL kind 2."
  (declare (fixnum off))
  (%kernel vec 0 off (length vec) 2)
  vec)
