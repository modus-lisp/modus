;;;; mac68k-cold.lisp -- COLD NAMES: symbol names a bare Mac reads from disk.
;;;;
;;;; *SYM-NAME-TABLE* maps the hash of every symbol-shaped name in the source
;;;; (12,000 of them) to its string, so SYMBOL-NAME can name a native symbol,
;;;; which holds only its hash.  On a Mac Classic's saved core it was 430 KB of
;;;; a 1.3 MB static heap, and a REPL session reads almost none of it.  So the
;;;; build also writes the names to a file of 512-byte pages sorted by hash
;;;; (build-cli-common.lisp, COLD NAMES), scripts/mac68k-core.py puts the file
;;;; on the disk after the code streams and the disk byte it starts at in the
;;;; static block's word #x10006044, and the freeze drops the table: from then
;;;; on a name the table would have given comes from %COLD-SYM-NAME -- the
;;;; page's first hashes are in the image (%COLD-NAMES-INDEX), so a miss is
;;;; one block read (SYSCALL3 service 1, translate-68k M68K-EMIT-MAC-SERVICE),
;;;; and a name read once is kept (*COLD-NAMES-MEMO*).
;;;;
;;;; Image source for the bare Mac only (build-cl-repl-common.lisp); the
;;;; defaults it overrides are in cl-packages.lisp.

(defvar *cold-names-firsts* nil)
(defvar *cold-names-memo* nil)

(defun %cold-names-firsts ()
  "Each page's first hash, from %COLD-NAMES-INDEX's 8 hex digits a page."
  (or *cold-names-firsts*
      (let* ((s (%cold-names-index)) (n (floor (length s) 8)) (v (make-array n)) (i 0))
        (loop (when (>= i n) (return nil))
          (setf (svref v i) (parse-integer s :start (* i 8) :end (+ (* i 8) 8) :radix 16))
          (setq i (+ i 1)))
        (setq *cold-names-firsts* v))))

(defun %cold-names-ready-p ()
  "Can names come from disk?  The core builder said where they are, and this
   image hashes a name as the build that wrote them did."
  (and (> (mem-ref #x10006044 :u32) 0)
       (eql (compute-name-hash "CAR") (%cold-names-check))))

(defun %cold-sym-name (h)
  "The name whose hash is H, from the cold names on disk; NIL when there is none
   or no disk to read it from."
  (when (and (fixnump h) (> h 0) (%cold-names-ready-p))
    (or (and *cold-names-memo* (gethash h *cold-names-memo*))
        (let* ((v (%cold-names-firsts)) (n (length v)))
          (when (and (> n 0) (>= h (svref v 0)))
            ;; the last page whose first hash is <= H
            (let ((lo 0) (hi (- n 1)))
              (loop (when (>= lo hi) (return nil))
                (let ((mid (ash (+ lo hi 1) -1)))
                  (if (<= (svref v mid) h) (setq lo mid) (setq hi (- mid 1)))))
              (let ((buf *io-buf-addr*))
                (when (eql (syscall3 1 (+ (mem-ref #x10006044 :u32) (* lo 512)) buf 512) 0)
                  (let ((i 0) (found nil))
                    (loop
                      (when (> i 507) (return nil))
                      (let ((k (+ (* (mem-ref (+ buf i) :u8) 16777216)
                                  (* (mem-ref (+ buf i 1) :u8) 65536)
                                  (* (mem-ref (+ buf i 2) :u8) 256)
                                  (mem-ref (+ buf i 3) :u8)))
                            (len (mem-ref (+ buf i 4) :u8)))
                        (when (eql k 0) (return nil))
                        (when (eql k h)
                          (let ((s (%make-base-string len)) (j 0))
                            (loop (when (>= j len) (return nil))
                              (%u8-set s j (mem-ref (+ buf i 5 j) :u8))
                              (setq j (+ j 1)))
                            (setq found s))
                          (return nil))
                        (setq i (+ i 5 len))))
                    (when found
                      (unless *cold-names-memo*
                        (setq *cold-names-memo* (make-hash-table)))
                      (puthash h *cold-names-memo* found))
                    found)))))))))

(defun %cold-names-drop ()
  "At the freeze of the CORE RUN (the SAVE flag, #x1000602C): drop
   *SYM-NAME-TABLE*, whose names the saved core's disk carries cold
   (mac68k-core.py writes them and #x10006044).  Its strings that something
   else holds (a symbol's cached name, the function table's keys) stay.  A boot
   from scratch keeps the table: its disk has no cold names."
  (when (and (eql (mem-ref #x1000602C :u32) 1)
             (eql (compute-name-hash "CAR") (%cold-names-check)))
    (setq *sym-name-table* nil)))
