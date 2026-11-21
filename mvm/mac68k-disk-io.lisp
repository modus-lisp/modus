;;;; mac68k-disk-io.lisp -- a disk block in or out of a byte vector, on a bare Mac.
;;;;
;;;; Image (kernel) source, compiled native: the FLOPPY package
;;;; (mvm/mac68k-files.lisp), which the core run LOADs and so runs as bytecode,
;;;; does a sector in one call here instead of 512 interpreted MEM-REFs (measured
;;;; on MAME's SE/30: a MOUNT and a DIR spent minutes in the interpreter).  The
;;;; bytes go through *IO-BUF-ADDR* + 8 KB -- the cold names read its first block
;;;; -- and nothing between the copy and the SYSCALL3 allocates, so a collection
;;;; cannot move VEC under it.  translate-68k's Mac disk service: 5 selects the
;;;; drive, 6 reads, 7 writes (_Write).

(defun %mac-disk-io (svc drive offset vec)
  "SVC 6: read the 512-byte block at byte OFFSET of DRIVE into VEC; 7: write VEC
   there.  0, or the Device Manager's error."
  (let ((b (+ *io-buf-addr* 8192)))
    (syscall3 5 drive 0 0)
    (when (eql svc 7)
      (dotimes (i 512) (setf (mem-ref (+ b i) :u8) (aref vec i))))
    (let ((r (syscall3 svc offset b 512)))
      (when (and (eql r 0) (eql svc 6))
        (dotimes (i 512) (setf (aref vec i) (mem-ref (+ b i) :u8))))
      r)))
