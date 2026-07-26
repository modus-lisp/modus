;;;; hosted-storage.lisp — block storage + durability for the hosted CLI.
;;;;
;;;; cl-fileio already gives the hosted ./modus everything for host FILESYSTEM
;;;; access — open/read/write/close, lseek (file-position), stat/fstat,
;;;; ftruncate, mkdir/unlink/rename/getdents64, directory/probe/delete/rename.
;;;; What was missing for a CRASH-SAFE BLOCK STORE (pagetree's COW B+tree,
;;;; cabinet's crash-atomic FS, cl-consensus's chain store) is two things:
;;;;   1. fsync — the durability barrier (force dirty data to stable storage).
;;;;   2. POSITIONED BULK block I/O — the CL stream write path is one write(2)
;;;;      per char (see %fs-flush's note), unusable for 4 KiB blocks.
;;;;
;;;; This layer is RAW-FD based (open O_RDWR|O_CREAT, lseek+read/write a whole
;;;; block, fsync, ftruncate) — exactly the primitive a no-FFI/no-mmap B+tree
;;;; store wants, not a boxed CL stream.  All ops are <= 3-arg Linux syscalls
;;;; (open=2 read=0 write=1 close=3 lseek=8 fsync=74 fdatasync=75 ftruncate=77
;;;; fstat=5), so they ride the existing syscall3 trap — CLI-only, no gate.

;;; Dedicated block-transfer scratch: a mapped page range below the heap,
;;; clear of *cstr-scratch* (#x0FE0…) / *io-buf-addr* (#x0FF0…) / the socket
;;; buffers.  Verified mapped for MiBs (0x0FB..0x0FF).
(defvar *block-scratch* #x0FC00000)
(defvar *block-scratch-max* 65536)      ; 64 KiB copied per syscall; larger chunks loop

;;; ---- raw syscall wrappers not already in cl-fileio ----
(defun %sys-fsync (fd)        (syscall3 74 fd 0 0))
(defun %sys-fdatasync (fd)    (syscall3 75 fd 0 0))
(defun %sys-ftruncate (fd n)  (syscall3 77 fd n 0))

;;; ---- raw-fd block store API ----

;;; Open PATH for read/write positioned block I/O (O_RDWR|O_CREAT, mode 0644).
;;; Returns a raw fd, or -1 on failure.  Does NOT truncate an existing store.
(defun block-open (path)
  (%string-to-cstr path *cstr-scratch*)
  (let ((fd (syscall3 2 *cstr-scratch* 66 420)))   ; 66 = O_RDWR|O_CREAT, 420 = 0644
    (if (< fd 0) -1 fd)))

(defun block-close (fd) (%sys-close fd))

;;; Write LEN bytes of ARR (an (unsigned-byte 8) array) to FD at byte OFFSET.
;;; Returns bytes written, or negative on error.  Not durable until FSYNC.
(defun block-pwrite (fd offset arr len)
  (let ((scr *block-scratch*) (done 0))
    (loop
      (when (>= done len) (return done))
      (let ((chunk (- len done)))
        (when (> chunk *block-scratch-max*) (setq chunk *block-scratch-max*))
        (let ((i 0))
          (loop
            (when (>= i chunk) (return nil))
            (setf (mem-ref (+ scr i) :u8) (aref arr (+ done i)))
            (setq i (+ i 1))))
        (%sys-lseek fd (+ offset done) 0)            ; SEEK_SET
        (let ((n (%sys-write-raw fd scr chunk)))
          (when (< n 1) (return (if (= done 0) n done)))
          (setq done (+ done n)))))))

;;; Read up to MAX bytes from FD at byte OFFSET into ARR.  Returns bytes read
;;; (0 = EOF at offset), or negative on error.  A short read (< requested)
;;; means EOF was hit and stops the loop.
(defun block-pread (fd offset arr max)
  (let ((scr *block-scratch*) (done 0))
    (loop
      (when (>= done max) (return done))
      (let ((chunk (- max done)))
        (when (> chunk *block-scratch-max*) (setq chunk *block-scratch-max*))
        (%sys-lseek fd (+ offset done) 0)
        (let ((n (%sys-read-raw fd scr chunk)))
          (when (< n 1) (return (if (= done 0) n done)))
          (let ((i 0))
            (loop
              (when (>= i n) (return nil))
              (aset arr (+ done i) (mem-ref (+ scr i) :u8))
              (setq i (+ i 1))))
          (setq done (+ done n))
          (when (< n chunk) (return done)))))))

;;; Durability barrier — force FD's writes to stable storage.  Call after a
;;; block/commit write.  Returns T.  fdatasync skips metadata (cheaper when
;;; only the data, not the size/mtime, must be durable).
(defun block-fsync (fd)     (%sys-fsync fd) t)
(defun block-fdatasync (fd) (%sys-fdatasync fd) t)

;;; Set the store file's length to N bytes (grow sparse / shrink).
(defun block-truncate (fd n) (%sys-ftruncate fd n) t)

;;; Current store size in bytes (fstat).
(defun block-size (fd) (%sys-fstat-size fd))

;;; ---- CL-stream durability convenience (for stream-based callers) ----
;;; fsync a live file STREAM.  File-stream writes are already unbuffered
;;; (one write(2) per char), so no user-space flush precedes the barrier.
(defun stream-fsync (stream)
  (if (and (streamp stream) (= (%stream-type stream) 9))
      (let ((fd (%fs-fd stream)))
        (if (< fd 0) nil (progn (%sys-fsync fd) t)))
      nil))

;;; ---- self-test: durable positioned block round-trip ----
;;; Write a 4 KiB pattern block at a sparse offset, fsync, close; reopen,
;;; read it back, compare; truncate; report.  Returns the number of matching
;;; bytes (4096 = clean round-trip) or a negative error code.
(defun block-store-selftest (path)
  (let ((blk 4096) (off 8192))
    (let ((fd (block-open path)))
      (if (< fd 0)
          -1
          (let ((wbuf (make-array blk)))
            ;; deterministic pattern: byte i = (i * 37 + 11) mod 256
            (let ((i 0))
              (loop
                (when (>= i blk) (return nil))
                (aset wbuf i (logand (+ (* i 37) 11) 255))
                (setq i (+ i 1))))
            (block-pwrite fd off wbuf blk)
            (block-fsync fd)
            (block-close fd)
            ;; reopen + read back
            (let ((fd2 (block-open path)) (rbuf (make-array blk)))
              (block-pread fd2 off rbuf blk)
              (block-truncate fd2 (+ off blk))
              (let ((sz (block-size fd2)))
                (block-close fd2)
                (let ((match 0) (i 0))
                  (loop
                    (when (>= i blk) (return nil))
                    (when (= (aref rbuf i)
                             (logand (+ (* i 37) 11) 255))
                      (setq match (+ match 1)))
                    (setq i (+ i 1)))
                  (list match sz)))))))))
