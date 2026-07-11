;;;; tar.lisp — POSIX ustar archive reader (pure CL, no FFI).
;;;;
;;;; Parses a ustar archive held in a byte vector (an (unsigned-byte 8) vector,
;;;; e.g. the output of chipz:decompress on a .tar.gz).  Handles regular files
;;;; (typeflag #\0 or NUL) and directories (typeflag #\5); other entry types are
;;;; skipped gracefully.  Two consecutive all-zero 512-byte blocks terminate.
;;;;
;;;; Public API:
;;;;   (tar-extract bytes)         -> list of (name . content-byte-vector)
;;;;                                  (directories are omitted from the result)
;;;;   (tar-list    bytes)         -> list of entry names (files AND dirs)
;;;;   (tar-do-entries bytes fn)   -> call (fn name typeflag content-or-nil) per
;;;;                                  entry; content is a fresh byte vector for
;;;;                                  regular files, NIL otherwise.
;;;;
;;;; Header field offsets within a 512-byte block (POSIX ustar):
;;;;   0   name           (100 bytes, NUL-padded)
;;;;   124 size           (12 bytes, octal ASCII, NUL/space terminated)
;;;;   156 typeflag       (1 byte:  #\0 or NUL = file, #\5 = directory)
;;;;   257 magic          ("ustar\0" or "ustar  ")
;;;;   345 prefix         (155 bytes, prepended to name with a "/" separator)

(defvar *tar-block-size* 512)

(defun %tar-byte (bytes i)
  "Element I of BYTES as a fixnum in 0..255."
  (aref bytes i))

(defun %tar-field-string (bytes off len)
  "Extract a NUL-terminated (or LEN-bounded) ASCII field into a fresh string.
   Stops at the first NUL byte.  Two passes (count then fill) so we never
   grow a string with CONCATENATE in a loop (that pattern wedged on the
   bare-metal image; a preallocated string with char stores is robust)."
  (let ((slen 0) (i 0))
    (loop
      (when (>= i len) (return))
      (when (= (%tar-byte bytes (+ off i)) 0) (return))
      (setq slen (+ slen 1))
      (setq i (+ i 1)))
    (let ((out (make-string slen)) (j 0))
      (loop
        (when (>= j slen) (return))
        (setf (char out j) (code-char (%tar-byte bytes (+ off j))))
        (setq j (+ j 1)))
      out)))

(defun %tar-parse-octal (bytes off len)
  "Parse an octal ASCII field (LEN bytes at OFF) into an integer.
   Leading spaces/NULs are skipped; parsing stops at the first space or NUL
   after digits begin.  An empty field parses as 0."
  (let ((val 0)
        (i 0)
        (started nil))
    (loop
      (when (>= i len) (return))
      (let ((b (%tar-byte bytes (+ off i))))
        (cond
          ((and (>= b 48) (<= b 55))          ; #\0 .. #\7
           (setq started t)
           (setq val (+ (* val 8) (- b 48))))
          ((or (= b 32) (= b 0))              ; space or NUL
           (when started (return)))           ; terminator after digits
          (t nil)))                            ; ignore any stray byte
      (setq i (+ i 1)))
    val))

(defun %tar-zero-block-p (bytes off)
  "True if the 512-byte block at OFF is entirely zero (or runs past end)."
  (let ((i 0) (allzero t) (n (length bytes)))
    (loop
      (when (>= i 512) (return))
      (let ((k (+ off i)))
        (when (< k n)
          (when (/= (%tar-byte bytes k) 0)
            (setq allzero nil)
            (return))))
      (setq i (+ i 1)))
    allzero))

(defun %tar-slice (bytes off len)
  "Return a fresh vector = BYTES[OFF .. OFF+LEN).  Plain (make-array len) — the
   bare-metal image's make-array wedges on the :element-type '(unsigned-byte 8)
   keyword; a generic array holds byte fixnums fine for the tar/string readers."
  (let ((out (make-array len))
        (i 0))
    (loop
      (when (>= i len) (return))
      (aset out i (%tar-byte bytes (+ off i)))
      (setq i (+ i 1)))
    out))

(defun %tar-round-up-block (n)
  "Round N up to the next multiple of *tar-block-size*."
  (let ((r (mod n 512)))
    (if (= r 0) n (+ n (- 512 r)))))

(defun tar-do-entries (bytes fn)
  "Walk the ustar archive in BYTES.  For each entry call
   (funcall FN name typeflag content) where CONTENT is a fresh byte vector for
   regular files and NIL for directories/other types.  Returns NIL."
  (let ((off 0)
        (n (length bytes))
        (zero-run 0))
    (loop
      ;; Need a full header block, else stop.
      (when (> (+ off 512) n) (return))
      (if (%tar-zero-block-p bytes off)
          (progn
            (setq zero-run (+ zero-run 1))
            (setq off (+ off 512))
            ;; Two consecutive zero blocks = end of archive.
            (when (>= zero-run 2) (return)))
          (progn
            (setq zero-run 0)
            (let* ((name   (%tar-field-string bytes (+ off 0) 100))
                   (prefix (%tar-field-string bytes (+ off 345) 155))
                   (size   (%tar-parse-octal bytes (+ off 124) 12))
                   (tflag  (%tar-byte bytes (+ off 156)))
                   (full   (if (> (length prefix) 0)
                               (concatenate 'string prefix "/" name)
                               name))
                   (data-off (+ off 512)))
              (cond
                ;; Regular file: typeflag #\0 (48) or NUL (0).
                ((or (= tflag 48) (= tflag 0))
                 (let ((content (%tar-slice bytes data-off size)))
                   (funcall fn full 48 content)
                   ))
                ;; Directory: typeflag #\5 (53).
                ((= tflag 53)
                 (funcall fn full 53 nil))
                ;; Anything else (symlink 50, hardlink 49, longname 'L', etc.):
                ;; skip its data but report it so callers can log if they wish.
                (t
                 (funcall fn full tflag nil)))
              ;; Advance past header + padded data.
              (setq off (+ data-off (%tar-round-up-block size)))))))
    nil))

(defun tar-extract (bytes)
  "Return a list of (NAME . CONTENT-BYTE-VECTOR) for every regular file in the
   ustar archive BYTES.  Directories and other entry types are omitted."
  (let ((acc nil))
    (tar-do-entries
     bytes
     (lambda (name tflag content)
       (when (and (= tflag 48) content)
         (setq acc (cons (cons name content) acc)))))
    (nreverse acc)))

(defun tar-list (bytes)
  "Return a list of every entry name in the ustar archive BYTES (files+dirs)."
  (let ((acc nil))
    (tar-do-entries
     bytes
     (lambda (name tflag content)
       (declare (ignore tflag content))
       (setq acc (cons name acc))))
    (nreverse acc)))

;;; --- Byte-content -> string helper (ASCII/Latin-1) --------------------------

(defun tar-bytes-to-string (bytes)
  "Coerce a byte vector to a string, one char per byte (Latin-1).  Preallocate
   + char-store (no loop-growing CONCATENATE — see %tar-field-string), then
   SUBSEQ to normalize into a fresh simple string (make-string output fed
   straight to make-string-input-stream wedged the bare-metal reader; the
   subseq copy reads cleanly)."
  (let* ((n (length bytes))
         (out (make-string n))
         (i 0))
    (loop
      (when (>= i n) (return))
      ;; Store the raw char CODE directly with %prim-aset — the string-input
      ;; reader's fast path uses %prim-aref (raw code), and a public
      ;; (setf (char ...)) round-trip on a byte value read via generic AREF
      ;; produced a string the reader wedged on.  %prim-aset is the internal
      ;; code-store per CLAUDE.md's string-element contract.
      (%prim-aset out i (%tar-byte bytes i))
      (setq i (+ i 1)))
    out))
