;;;; cl-fileio.lisp — File I/O: Linux syscalls, file streams, open/close, pathnames
;;;; Part of the Modus CL runtime. Depends on cl-streams.lisp.

;;; ============================================================
;;; Layer 7: File I/O — Linux syscall backend
;;; ============================================================
;;; File stream data layout:
;;;   stream = (cons 7770001 (cons 9 (cons fd (cons dir (cons pos (cons buf (cons buf-pos (cons buf-len (cons closed nil)))))))))
;;;   fd       = Linux file descriptor (fixnum, -1 if not open)
;;;   dir      = 0=input, 1=output, 2=io
;;;   pos      = current file position (bytes)
;;;   buf      = string (4096 bytes) used as I/O buffer
;;;   buf-pos  = current read position in buffer
;;;   buf-len  = number of valid bytes in buffer
;;;   closed   = nil or t

;;; Fixed memory addresses for raw C-string scratch area
;;; (well above heap, in unmapped zone — we mmap it lazily via write)
(defvar *cstr-scratch* #x1DF00000)  ; C-string scratch: up to 4096 bytes
(defvar *io-buf-addr*  #x1DE00000)  ; Raw I/O buffer: 4096 bytes

;;; Linux open flags
(defun %o-rdonly ()   0)
(defun %o-wronly ()   1)
(defun %o-rdwr ()     2)
(defun %o-creat ()  #x40)
(defun %o-trunc ()  #x200)
(defun %o-append () #x400)
(defun %o-excl ()   #x80)

;;; Mmap the scratch area once at startup
(defvar *scratch-mmapped* nil)
(defun %ensure-scratch-mmapped ()
  (when (null *scratch-mmapped*)
    (setq *scratch-mmapped* t)
    ;; mmap #x1DE00000 with 2 pages (8KB) for I/O buf + C-string
    (syscall3 9 #x1DE00000 8192)  ;; hint addr (already tagged — syscall3 untags)
    ;; Actually use raw syscall for mmap with full args:
    ;; We can't call syscall3 with 6 args. Instead, use the fixed buffers
    ;; already mapped by the Linux ELF entry (heap is 896MB at 0x10000000).
    ;; Since our heap is 896MB (0x10000000-0x48000000), addresses
    ;; 0x1DE00000 and 0x1DF00000 are WITHIN the heap region — they're already mapped!
    nil))

;;; Write a Lisp string as null-terminated C string at a fixed address.
;;; Uses mem-ref :u8 stores (which untag the byte value).
(defun %string-to-cstr (str addr)
  "Write STR as a null-terminated C string at byte address ADDR."
  (let ((len (length str))
        (i 0))
    (loop
      (when (>= i len) (return nil))
      (setf (mem-ref (+ addr i) :u8) (aref str i))
      (setq i (+ i 1)))
    ;; Null terminator
    (setf (mem-ref (+ addr len) :u8) 0)
    addr))

;;; Low-level file syscalls.
;;; syscall3 takes tagged fixnum args, untags them before syscall.
;;; For addresses (path pointer), we pass the raw address as a fixnum — syscall3 untags (SHR 1).
;;; BUT: addresses like 0x1DF00000 when tagged (SHL 1) = 0x3BE00000 which is a valid fixnum.
;;; syscall3 untags: 0x3BE00000 SHR 1 = 0x1DF00000. Correct!

(defun %sys-open-rdonly (path-str)
  "Open file for reading. Returns fd (fixnum) or negative errno."
  (%string-to-cstr path-str *cstr-scratch*)
  ;; syscall3: num=2(open), arg1=path-ptr(tagged addr), arg2=flags=0(O_RDONLY), arg3=0
  (syscall3 2 *cstr-scratch* 0 0))

(defun %sys-open-wronly (path-str)
  "Open file for writing (create/truncate). Returns fd or negative errno."
  (%string-to-cstr path-str *cstr-scratch*)
  ;; O_WRONLY|O_CREAT|O_TRUNC = 1|0x40|0x200 = 0x241 = 577
  (syscall3 2 *cstr-scratch* 577 420))  ; 420 = 0644 octal

(defun %sys-open-append (path-str)
  "Open file for appending. Returns fd or negative errno."
  (%string-to-cstr path-str *cstr-scratch*)
  ;; O_WRONLY|O_CREAT|O_APPEND = 1|0x40|0x400 = 0x441 = 1089
  (syscall3 2 *cstr-scratch* 1089 420))

(defun %sys-open-rdwr (path-str)
  "Open file for read+write. Returns fd or negative errno."
  (%string-to-cstr path-str *cstr-scratch*)
  ;; O_RDWR|O_CREAT = 2|0x40 = 66
  (syscall3 2 *cstr-scratch* 66 420))

(defun %sys-open-create-excl (path-str)
  "Open/create file exclusively (error if exists). Returns fd or negative errno."
  (%string-to-cstr path-str *cstr-scratch*)
  ;; O_WRONLY|O_CREAT|O_EXCL = 1|0x40|0x80 = 0xC1 = 193
  (syscall3 2 *cstr-scratch* 193 420))

(defun %sys-close (fd)
  "Close file descriptor."
  (syscall3 3 fd 0 0))

(defun %sys-read-raw (fd buf-addr count)
  "Read COUNT bytes from FD into buf at BUF-ADDR. Returns bytes read or negative."
  (syscall3 0 fd buf-addr count))

(defun %sys-write-raw (fd buf-addr count)
  "Write COUNT bytes from buf at BUF-ADDR to FD. Returns bytes written or negative."
  (syscall3 1 fd buf-addr count))

(defun %sys-lseek (fd offset whence)
  "Seek FD. whence: 0=SEEK_SET, 1=SEEK_CUR, 2=SEEK_END."
  (syscall3 8 fd offset whence))

(defun %sys-unlink (path-str)
  "Delete a file."
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 87 *cstr-scratch* 0 0))

(defun %sys-rename (old-str new-str)
  "Rename a file."
  (%string-to-cstr old-str *cstr-scratch*)
  ;; Use a second scratch area 2048 bytes in
  (let ((new-addr (+ *cstr-scratch* 2048)))
    (%string-to-cstr new-str new-addr)
    (syscall3 82 *cstr-scratch* new-addr 0)))

(defun %sys-mkdir (path-str mode)
  "Create a directory."
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 83 *cstr-scratch* mode 0))

;;; stat(2) on Linux x64: syscall 4, fills struct stat (144 bytes)
;;; We only need st_size at offset 48 and st_mtime at offset 88.
;;; Use :u32 loads (tagged) for values that fit in 32 bits.
(defun %sys-stat-size (path-str)
  "Return file size in bytes (lower 32 bits), or -1 if not found."
  ;; Bind both addresses to locals before syscall3 to avoid register clobbering:
  ;; compile-syscall3 evaluates each arg into V4-V7; evaluating arg2 (io-buf global)
  ;; calls symbol-value which trashes V5 (RCX) = arg1's register.
  ;; By binding to locals first, the values sit in callee-saved/stack slots.
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 4 path-addr buf-addr 0)))
      (if (< ret 0)
          -1
          ;; st_size is at offset 48 in struct stat (little-endian, lower 32 bits)
          (mem-ref (+ buf-addr 48) :u32)))))

(defun %sys-stat-exists (path-str)
  "Return t if file exists, nil otherwise."
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 4 path-addr buf-addr 0)))
      (if (< ret 0) nil t))))

(defun %sys-stat-mtime (path-str)
  "Return file modification time (lower 32 bits of seconds since epoch), or 0."
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 4 path-addr buf-addr 0)))
      (if (< ret 0)
          0
          ;; st_mtim.tv_sec at offset 88 (lower 32 bits)
          (mem-ref (+ buf-addr 88) :u32)))))

;;; fstat(2) on Linux x64: syscall 5, fills struct stat
(defun %sys-fstat-size (fd)
  "Return file size for open fd (lower 32 bits), or -1."
  (let ((buf-addr *io-buf-addr*))
    (let ((ret (syscall3 5 fd buf-addr 0)))
      (if (< ret 0)
          -1
          (mem-ref (+ buf-addr 48) :u32)))))

;;; File stream constructor and accessors
;;; Data = (fd dir pos buf buf-pos buf-len closed)
;;; Encoded as a cons chain: (fd . (dir . (pos . (buf . (buf-pos . (buf-len . closed))))))

(defun %make-file-stream-full (fd dir)
  "Create a file stream with given fd and direction (0=in, 1=out, 2=io)."
  (let ((buf (%make-string-array 4096)))
    (%make-stream 9
      (cons fd (cons dir (cons 0 (cons buf (cons 0 (cons 0 nil)))))))))

(defun %make-file-stream ()
  "Create a closed/dummy file stream."
  (%make-stream 9 (cons -1 (cons 0 (cons 0 (cons nil (cons 0 (cons 0 t))))))))

(defun %fs-fd      (s) (car  (%stream-data s)))
(defun %fs-dir     (s) (cadr (%stream-data s)))
(defun %fs-pos-cell (s) (cddr (%stream-data s)))
(defun %fs-pos     (s) (car  (cddr (%stream-data s))))
(defun %fs-buf     (s) (cadr (cddr (%stream-data s))))
(defun %fs-bpos-cell (s) (cddr (cddr (%stream-data s))))
(defun %fs-bpos    (s) (car  (cddr (cddr (%stream-data s)))))
(defun %fs-blen-cell (s) (cdr (cddr (cddr (%stream-data s)))))
(defun %fs-blen    (s) (car  (cdr (cddr (cddr (%stream-data s))))))
(defun %fs-closed  (s) (cdr  (cdr (cddr (cddr (%stream-data s))))))

(defun %fs-set-pos  (s v) (set-car (cddr (%stream-data s)) v))
(defun %fs-set-bpos (s v) (set-car (cddr (cddr (%stream-data s))) v))
(defun %fs-set-blen (s v) (set-car (cdr (cddr (cddr (%stream-data s)))) v))
(defun %fs-set-closed (s v) (set-cdr (cdr (cddr (cddr (%stream-data s)))) v))

;;; *default-pathname-defaults* — base directory for relative pathnames
(defvar *default-pathname-defaults* "")

;;; Resolve a pathname (string or object) to an absolute path string.
(defun %strip-logical-host (path)
  "Strip logical host prefix like CLTEST: from path."
  (let ((len (length path))
        (i 0))
    (loop
      (when (>= i len) (return path))
      (when (= (aref path i) 58)  ; 58 = ':'
        ;; Found colon — strip everything up to and including it
        (return (%substring path (+ i 1) len)))
      (setq i (+ i 1)))))

(defun %resolve-path (filespec)
  "Convert filespec to a path string, prepending *default-pathname-defaults* if relative."
  (let ((path (cond
                ((stringp filespec) (%strip-logical-host filespec))
                (t (if filespec (%strip-logical-host (write-to-string filespec)) "")))))
    ;; If path is relative (doesn't start with /), prepend defaults
    (if (and (> (length path) 0) (= (aref path 0) 47))  ; 47 = #\/
        path
        (let ((base *default-pathname-defaults*))
          (if (and base (> (length base) 0))
              (let ((base-len (length base)))
                ;; Ensure base ends with /
                (if (= (aref base (- base-len 1)) 47)
                    (concatenate-strings base path)
                    (concatenate-strings base (concatenate-strings "/" path))))
              path)))))

;;; --- open function ---
;;; Args: filespec &key direction element-type if-exists if-does-not-exist external-format
;;; We use &rest + manual parsing (no &key in MVM)

(defun open (filespec &rest args)
  "Open a file. Returns a file stream or signals error.
   Options: :direction (:input/:output/:io/:probe),
            :if-exists (:error/:new-version/:rename/:supersede/:overwrite/:append/nil),
            :if-does-not-exist (:error/:create/nil),
            :element-type,
            :external-format"
  (let ((direction :input)
        (if-exists :new-version)
        (if-does-not-exist nil)  ; default: determined by direction below
        (element-type 'character)
        (if-does-not-exist-set nil)
        (cur args))
    ;; Parse keyword args from &rest
    (loop
      (when (null cur) (return nil))
      (let ((key (car cur))
            (val (cadr cur)))
        (cond
          ((eq key :direction) (setq direction val))
          ((eq key :if-exists) (setq if-exists val))
          ((eq key :if-does-not-exist)
           (setq if-does-not-exist val)
           (setq if-does-not-exist-set t))
          ((eq key :element-type) (setq element-type val))
          ;; :external-format, :class, etc. — ignore
          ))
      (setq cur (cddr cur)))
    ;; Apply ANSI defaults for :if-does-not-exist based on direction
    (unless if-does-not-exist-set
      (cond
        ((eq direction :input)  (setq if-does-not-exist :error))
        ((eq direction :output) (setq if-does-not-exist :create))
        ((eq direction :io)     (setq if-does-not-exist :create))
        (t (setq if-does-not-exist nil))))
    ;; Apply ANSI defaults for :if-exists based on direction
    (when (eq if-exists :new-version)
      (cond
        ((eq direction :input) (setq if-exists :overwrite))
        (t nil)))  ; keep :new-version (means :supersede in our impl)
    ;; Resolve path
    (let ((path (%resolve-path filespec)))
      ;; Determine open flags based on direction and options
      (cond
        ;; :probe — check existence, return nil if not found
        ((eq direction :probe)
         (if (%sys-stat-exists path)
             (%make-file-stream-full -1 0)  ; dummy open stream
             nil))
        ;; :input — read-only
        ((eq direction :input)
         (let ((fd (%sys-open-rdonly path)))
           (if (< fd 0)
               ;; File doesn't exist
               (cond
                 ((null if-does-not-exist) nil)
                 (t (error "Cannot open ~A for input" path)))
               (%make-file-stream-full fd 0))))
        ;; :output — write
        ((eq direction :output)
         (let ((exists (%sys-stat-exists path)))
           (cond
             ;; File exists — handle :if-exists
             (exists
              (cond
                ((null if-exists) nil)
                ((or (eq if-exists :error) (eq if-exists :new-version))
                 (error "File ~A already exists" path))
                ((or (eq if-exists :supersede) (eq if-exists :overwrite)
                     (eq if-exists :rename-and-delete) (eq if-exists :rename))
                 (let ((fd (%sys-open-wronly path)))
                   (if (< fd 0)
                       (error "Cannot open ~A for output" path)
                       (%make-file-stream-full fd 1))))
                ((eq if-exists :append)
                 (let ((fd (%sys-open-append path)))
                   (if (< fd 0)
                       (error "Cannot open ~A for append" path)
                       (%make-file-stream-full fd 1))))
                (t (let ((fd (%sys-open-wronly path)))
                     (if (< fd 0)
                         (error "Cannot open ~A for output" path)
                         (%make-file-stream-full fd 1))))))
             ;; File doesn't exist
             (t
              (cond
                ((null if-does-not-exist) nil)
                ((eq if-does-not-exist :error)
                 (error "File ~A does not exist" path))
                (t (let ((fd (%sys-open-wronly path)))
                     (if (< fd 0)
                         (error "Cannot create ~A" path)
                         (%make-file-stream-full fd 1)))))))))
        ;; :io — read/write
        ((eq direction :io)
         (let ((fd (%sys-open-rdwr path)))
           (if (< fd 0)
               (cond
                 ((null if-does-not-exist) nil)
                 (t (error "Cannot open ~A for io" path)))
               (%make-file-stream-full fd 2))))
        (t (error "Unknown :direction ~A" direction))))))

;;; --- close ---
(defun close (stream &rest args)
  "Close a stream. For file streams, closes the fd."
  (when (streamp stream)
    (when (= (%stream-type stream) 9)
      (let ((fd (%fs-fd stream)))
        (when (>= fd 0)
          ;; Flush output buffer if needed
          (%fs-flush stream)
          (%sys-close fd)
          ;; Mark as closed by setting fd to -1
          (set-car (%stream-data stream) -1)
          (%fs-set-closed stream t)))))
  t)

;;; Flush any pending output to file
(defun %fs-flush (stream)
  "Flush output buffer for file stream."
  (when (streamp stream)
    (when (= (%stream-type stream) 9)
      (let ((fd (%fs-fd stream))
            (dir (%fs-dir stream)))
        (when (and (>= fd 0) (> dir 0))
          ;; For output, write pending data
          ;; (currently we write char-by-char so nothing to flush)
          nil)))))

;;; --- File stream read-char ---
(defun %fs-read-char (stream eof-error-p eof-value)
  "Read one character from a file stream using buffered I/O."
  (let ((fd (%fs-fd stream)))
    (if (< fd 0)
        (if eof-error-p (error "end of file") eof-value)
        (let ((bpos (%fs-bpos stream))
              (blen (%fs-blen stream)))
          (if (< bpos blen)
              ;; Buffer has data
              (let ((ch (code-char (aref (%fs-buf stream) bpos))))
                (%fs-set-bpos stream (+ bpos 1))
                (%fs-set-pos stream (+ (%fs-pos stream) 1))
                ch)
              ;; Need to refill buffer
              (let ((n (%sys-read-raw fd *io-buf-addr* 4096)))
                (if (<= n 0)
                    ;; EOF
                    (if eof-error-p (error "end of file") eof-value)
                    ;; Copy io-buf to stream's buffer
                    ;; NOTE: aset with variable index has dest=nil bug when non-last form.
                    ;; Workaround: wrap in let so dest = frame slot (spill register).
                    (let ((buf (%fs-buf stream))
                          (io-addr *io-buf-addr*)
                          (i 0))
                      (loop
                        (when (>= i n) (return nil))
                        (let ((dummy (aset buf i (mem-ref (+ io-addr i) :u8))))
                          (setq i (+ i 1))))
                      (%fs-set-bpos stream 0)
                      (%fs-set-blen stream n)
                      ;; Recurse to read first char
                      (%fs-read-char stream eof-error-p eof-value)))))))))

;;; --- File stream write-char ---
(defun %fs-write-char (code stream)
  "Write a char code to a file stream."
  (let ((fd (%fs-fd stream)))
    (when (>= fd 0)
      ;; Write single byte via io-buf
      (setf (mem-ref *io-buf-addr* :u8) code)
      (%sys-write-raw fd *io-buf-addr* 1)
      (%fs-set-pos stream (+ (%fs-pos stream) 1)))))

;;; --- File stream read-byte ---
(defun %fs-read-byte (stream eof-error-p eof-value)
  "Read one byte from a file stream."
  (let ((fd (%fs-fd stream)))
    (if (< fd 0)
        (if eof-error-p (error "end of file") eof-value)
        (let ((bpos (%fs-bpos stream))
              (blen (%fs-blen stream)))
          (if (< bpos blen)
              (let ((b (aref (%fs-buf stream) bpos)))
                (%fs-set-bpos stream (+ bpos 1))
                (%fs-set-pos stream (+ (%fs-pos stream) 1))
                b)
              (let ((n (%sys-read-raw fd *io-buf-addr* 4096)))
                (if (<= n 0)
                    (if eof-error-p (error "end of file") eof-value)
                    ;; Copy io-buf to stream's buffer
                    ;; NOTE: aset with variable index has dest=nil bug when non-last form.
                    ;; Workaround: wrap in let so dest = frame slot (spill register).
                    (let ((buf (%fs-buf stream))
                          (io-addr *io-buf-addr*)
                          (i 0))
                      (loop
                        (when (>= i n) (return nil))
                        (let ((dummy (aset buf i (mem-ref (+ io-addr i) :u8))))
                          (setq i (+ i 1))))
                      (%fs-set-bpos stream 0)
                      (%fs-set-blen stream n)
                      (%fs-read-byte stream eof-error-p eof-value)))))))))

;;; --- File stream write-byte ---
(defun %fs-write-byte (byte stream)
  "Write one byte to a file stream."
  (let ((fd (%fs-fd stream)))
    (when (>= fd 0)
      (setf (mem-ref *io-buf-addr* :u8) byte)
      (%sys-write-raw fd *io-buf-addr* 1)
      (%fs-set-pos stream (+ (%fs-pos stream) 1)))))

;;; --- file-length ---
(defun file-length (stream)
  "Return the length of a file stream in bytes."
  (if (not (streamp stream))
      (error "file-length: not a stream")
      (let ((ty (%stream-type stream)))
        (cond
          ((= ty 9)
           (let ((fd (%fs-fd stream)))
             (if (< fd 0)
                 (error "file-length: stream is closed")
                 (%sys-fstat-size fd))))
          ((= ty 5) ;; broadcast: use first file stream
           (let ((streams (%stream-data stream)))
             (if streams
                 (file-length (car streams))
                 (error "file-length: no streams"))))
          ;; Synonym: delegate
          ((= ty 7)
           (file-length (symbol-value (%stream-data stream))))
          (t (error "file-length: not a file stream"))))))

;;; --- file-position ---
(defun file-position (stream &rest args)
  "Get or set file position."
  (if (not (streamp stream))
      nil
      (let ((ty (%stream-type stream)))
        (cond
          ((= ty 9)
           (let ((fd (%fs-fd stream)))
             (if (< fd 0)
                 nil
                 (if (null args)
                     ;; Get current position (account for buffered bytes)
                     (let ((pos (%fs-pos stream))
                           (blen (%fs-blen stream))
                           (bpos (%fs-bpos stream)))
                       (- (+ pos bpos) bpos))  ;; actual: pos minus unconsumed buffer
                     ;; Set position
                     (let ((newpos (car args)))
                       (cond
                         ((eq newpos :start)
                          (%sys-lseek fd 0 0)
                          (%fs-set-pos stream 0)
                          (%fs-set-bpos stream 0)
                          (%fs-set-blen stream 0)
                          t)
                         ((eq newpos :end)
                          (let ((epos (%sys-lseek fd 0 2)))
                            (%fs-set-pos stream epos)
                            (%fs-set-bpos stream 0)
                            (%fs-set-blen stream 0)
                            t))
                         ((integerp newpos)
                          (%sys-lseek fd newpos 0)
                          (%fs-set-pos stream newpos)
                          (%fs-set-bpos stream 0)
                          (%fs-set-blen stream 0)
                          t)
                         (t nil)))))))
          ((= ty 7) ;; synonym
           (apply #'file-position (cons (symbol-value (%stream-data stream)) args)))
          (t (if args nil 0))))))

;;; --- Pathname functions ---
(defvar *filesystem* nil)  ;; alist of (path . content) for bare-metal use

;;; For testing: pathnames are just strings
(defun pathname (x)
  "Coerce X to a pathname (string in our implementation)."
  (cond
    ((stringp x) x)
    ((streamp x)
     (if (= (%stream-type x) 9)
         ""  ; file streams don't track their path currently
         ""))
    (t (if x (write-to-string x) ""))))

(defun pathnamep (x) (stringp x))

(defun namestring (x)
  "Return the namestring of a pathname."
  (cond
    ((stringp x) x)
    ((streamp x) "")
    (t "")))

(defun file-namestring (x)
  "Return just the filename part of a pathname."
  (let ((path (pathname x)))
    (let ((len (length path))
          (last-slash -1)
          (i 0))
      (loop
        (when (>= i len) (return nil))
        (when (= (aref path i) 47) (setq last-slash i))  ; 47 = /
        (setq i (+ i 1)))
      (if (= last-slash -1)
          path
          (%substring path (+ last-slash 1) len)))))

(defun directory-namestring (x)
  "Return just the directory part of a pathname."
  (let ((path (pathname x)))
    (let ((len (length path))
          (last-slash -1)
          (i 0))
      (loop
        (when (>= i len) (return nil))
        (when (= (aref path i) 47) (setq last-slash i))
        (setq i (+ i 1)))
      (if (= last-slash -1)
          ""
          (%substring path 0 (+ last-slash 1))))))

(defun host-namestring (x) "")
(defun enough-namestring (x &rest args) (namestring x))

(defun merge-pathnames (path &rest args)
  "Merge a path with optional defaults."
  (let ((p (namestring path))
        (defaults (if args (namestring (car args)) *default-pathname-defaults*)))
    (if (and (> (length p) 0) (= (aref p 0) 47))  ; absolute path
        p
        (let ((base (if defaults defaults "")))
          (if (> (length base) 0)
              (if (= (aref base (- (length base) 1)) 47)
                  (concatenate-strings base p)
                  (concatenate-strings base (concatenate-strings "/" p)))
              p)))))

(defun make-pathname (&rest args)
  "Build a pathname from components."
  (let ((dir nil) (name nil) (type nil) (host nil)
        (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((key (car cur)) (val (cadr cur)))
        (cond
          ((eq key :directory) (setq dir val))
          ((eq key :name) (setq name val))
          ((eq key :type) (setq type val))
          ((eq key :host) (setq host val))
          ((eq key :device) nil)  ; ignore
          ((eq key :version) nil)))
      (setq cur (cddr cur)))
    ;; Build path string from components
    (let ((result ""))
      (when dir
        (cond
          ((stringp dir)
           (setq result dir)
           (when (and (> (length dir) 0)
                      (not (= (aref dir (- (length dir) 1)) 47)))
             (setq result (concatenate-strings result "/"))))
          ((consp dir)
           ;; (:absolute "part1" "part2") or (:relative "part1")
           (when (consp dir)
             (let ((rel (car dir))
                   (parts (cdr dir)))
               (when (eq rel :absolute)
                 (setq result "/"))
               (dolist (p parts)
                 (when (stringp p)
                   (setq result (concatenate-strings result p))
                   (setq result (concatenate-strings result "/")))))))))
      (when name
        (setq result (concatenate-strings result name)))
      (when type
        (setq result (concatenate-strings result "."))
        (setq result (concatenate-strings result type)))
      result)))

(defun pathname-directory (x)
  "Extract directory component."
  (let ((path (namestring x)))
    (let ((len (length path))
          (last-slash -1)
          (i 0))
      (loop
        (when (>= i len) (return nil))
        (when (= (aref path i) 47) (setq last-slash i))
        (setq i (+ i 1)))
      (if (= last-slash -1)
          nil
          (list :absolute (%substring path 0 last-slash))))))

(defun pathname-name (x)
  "Extract file name (without extension)."
  (let ((fname (file-namestring x)))
    (let ((len (length fname))
          (last-dot -1)
          (i 0))
      (loop
        (when (>= i len) (return nil))
        (when (= (aref fname i) 46) (setq last-dot i))  ; 46 = .
        (setq i (+ i 1)))
      (if (= last-dot -1)
          fname
          (%substring fname 0 last-dot)))))

(defun pathname-type (x)
  "Extract file extension."
  (let ((fname (file-namestring x)))
    (let ((len (length fname))
          (last-dot -1)
          (i 0))
      (loop
        (when (>= i len) (return nil))
        (when (= (aref fname i) 46) (setq last-dot i))
        (setq i (+ i 1)))
      (if (= last-dot -1)
          nil
          (%substring fname (+ last-dot 1) len)))))

(defun pathname-host (x) nil)
(defun pathname-device (x) nil)
(defun pathname-version (x) :unspecific)

(defun parse-namestring (thing &rest args)
  "Parse a namestring. Returns (values pathname position)."
  (values (namestring thing) (length (namestring thing))))

(defun wild-pathname-p (x &rest args) nil)
(defun pathname-match-p (path wild) nil)
(defun translate-pathname (source from-wild to-wild) source)
(defun translate-logical-pathname (x) (pathname x))
(defun logical-pathname (x) (pathname x))
(defun logical-pathname-translations (host) nil)
(defun user-homedir-pathname () "/root/")

;;; --- probe-file ---
(defun probe-file (x)
  "Return truename if file exists, nil otherwise."
  (let ((path (%resolve-path (pathname x))))
    (if (%sys-stat-exists path) path nil)))

;;; --- truename ---
(defun truename (x)
  "Return the truename of a file (simplified: just return path)."
  (let ((path (%resolve-path (pathname x))))
    (if (%sys-stat-exists path) path
        (error "File does not exist: ~A" path))))

;;; --- delete-file ---
(defun delete-file (x)
  "Delete a file."
  (let ((path (%resolve-path (pathname x))))
    (let ((ret (%sys-unlink path)))
      (if (< ret 0)
          (error "Cannot delete ~A" path)
          t))))

;;; --- rename-file ---
(defun rename-file (old new)
  "Rename a file. Returns (values new-truename old-truename new-truename)."
  (let ((old-path (%resolve-path (pathname old)))
        (new-path (%resolve-path (pathname new))))
    (let ((ret (%sys-rename old-path new-path)))
      (if (< ret 0)
          (error "Cannot rename ~A to ~A" old-path new-path)
          (values new-path old-path new-path)))))

;;; --- ensure-directories-exist ---
(defun ensure-directories-exist (pathspec &rest args)
  "Ensure all directories in path exist. Returns (values pathspec created-p)."
  (let ((path (%resolve-path (pathname pathspec))))
    ;; Simple: try to mkdir -p by creating each component
    (let ((len (length path))
          (i 1))
      (loop
        (when (>= i len) (return nil))
        (when (= (aref path i) 47)  ; /
          (let ((dir (%substring path 0 i)))
            (%sys-mkdir dir 493)))  ; 493 = 0755
        (setq i (+ i 1)))
      (values pathspec t))))

;;; --- file-write-date ---
(defun file-write-date (x)
  "Return file modification time as universal time (seconds since 1900-01-01)."
  (let ((path (%resolve-path (pathname x))))
    ;; Unix epoch (1970-01-01) to CL universal time (1900-01-01) offset:
    ;; 70 years * 365.25 days * 86400 sec ≈ 2208988800
    (let ((mtime (%sys-stat-mtime path)))
      (if (= mtime 0) nil (+ mtime 2208988800)))))

;;; --- file-author ---
(defun file-author (x) nil)

;;; --- directory ---
(defun directory (x &rest args) nil)

;;; --- with-open-stream ---
(defun %with-open-stream-fn (stream thunk)
  "Invoke THUNK with STREAM, then close it."
  (let ((result (funcall thunk stream)))
    (close stream)
    result))

;;; --- read-byte (extended for file streams) ---
(defun read-byte (stream &rest args)
  "Read one byte from stream."
  (let ((eof-error-p (if args (car args) t))
        (eof-value (if (cdr args) (cadr args) nil)))
    (let ((s (if (streamp stream) stream nil)))
      (if (null s)
          (if eof-error-p (error "end of file") eof-value)
          (let ((ty (%stream-type s)))
            (cond
              ((= ty 9) (%fs-read-byte s eof-error-p eof-value))
              (t (if eof-error-p (error "end of file") eof-value))))))))

;;; --- write-byte (extended for file streams) ---
(defun write-byte (byte stream)
  "Write one byte to stream."
  (if (streamp stream)
      (let ((ty (%stream-type stream)))
        (cond
          ((= ty 9) (%fs-write-byte byte stream))
          (t nil)))
      nil)
  byte)

;;; Helper: concatenate two strings
(defun concatenate-strings (a b)
  "Concatenate two strings."
  (let ((la (length a))
        (lb (length b)))
    (let ((result (%make-string-array (+ la lb)))
          (i 0))
      (loop
        (when (>= i la) (return nil))
        (aset result i (aref a i))
        (setq i (+ i 1)))
      (let ((j 0))
        (loop
          (when (>= j lb) (return nil))
          (aset result (+ la j) (aref b j))
          (setq j (+ j 1))))
      result)))

;;; --- Stream type predicate for file-stream ---
(defun file-stream-p (s)
  "Return t if S is a file stream."
  (if (streamp s) (= (%stream-type s) 9) nil))

;;; file-string-length: for file streams, just return string length
(defun file-string-length (s str)
  (if (stringp str) (array-length str) 1))

;;; --- Stream type dispatchers extended for file streams ---

;;; --- Stream accessor stubs ---


(defun echo-stream-input-stream (s) (car (%stream-data s)))
(defun echo-stream-output-stream (s) (cdr (%stream-data s)))
(defun two-way-stream-input-stream (s) (car (%stream-data s)))
(defun two-way-stream-output-stream (s) (cdr (%stream-data s)))
(defun broadcast-stream-streams (s) (%stream-data s))
(defun concatenated-stream-streams (s) (car (%stream-data s)))
(defun synonym-stream-symbol (s) (%stream-data s))

;;; --- Stream designator resolution ---

(defun %resolve-input-stream (stream)
  "Resolve a stream designator to an actual input stream.
   nil -> *standard-input*, t -> *terminal-io* input side."
  (cond
    ((null stream) *standard-input*)
    ((eq stream t) (if (streamp *terminal-io*)
                       (two-way-stream-input-stream *terminal-io*)
                       *terminal-io*))
    ((streamp stream)
     (let ((ty (%stream-type stream)))
       (cond
         ((= ty 4) (two-way-stream-input-stream stream))  ;; two-way -> input side
         ((= ty 3) stream)   ;; echo stream stays as-is
         (t stream))))
    (t stream)))

(defun %resolve-output-stream (stream)
  "Resolve a stream designator to an actual output stream.
   nil -> *standard-output*, t -> *terminal-io* output side."
  (cond
    ((null stream) *standard-output*)
    ((eq stream t) (if (streamp *terminal-io*)
                       (two-way-stream-output-stream *terminal-io*)
                       *terminal-io*))
    ((streamp stream)
     (let ((ty (%stream-type stream)))
       (cond
         ((= ty 4) (two-way-stream-output-stream stream))  ;; two-way -> output side
         ((= ty 3) stream)   ;; echo stream stays as-is
         (t stream))))
    (t stream)))

;;; --- Core read-char ---

(defun read-char (&rest args)
  "Read one character from stream. Returns character object."
  (let ((stream-arg (if args (car args) nil))
        (eof-error-p (if (cdr args) (cadr args) t))
        (eof-value (if (cddr args) (caddr args) nil)))
    (let ((s (%resolve-input-stream stream-arg)))
      (%read-char-from-stream s eof-error-p eof-value))))

(defun %read-char-from-stream (s eof-error-p eof-value)
  "Read one character from a resolved stream."
  (if (not (streamp s))
      ;; Not a stream - signal error or return eof-value
      (if eof-error-p (error "end of file") eof-value)
      (let ((ty (%stream-type s)))
        (cond
          ;; String-input stream
          ((= ty 1)
           (let ((data (%stream-data s)))
             (let ((str (car data))
                   (pos-cell (cdr data)))
               ;; Check unread-char first
               (let ((unread (cdr pos-cell)))
                 (if unread
                     (progn
                       (set-cdr pos-cell nil)
                       unread)
                     ;; Read from string
                     (let ((pos (car pos-cell)))
                       (if (>= pos (length str))
                           ;; EOF
                           (if eof-error-p (error "end of file") eof-value)
                           (let ((ch (code-char (aref str pos))))
                             (set-car pos-cell (+ pos 1))
                             ch))))))))
          ;; Echo stream: read from input, echo to output
          ((= ty 3)
           (let ((data (%stream-data s)))
             ;; Check for unread char on the echo stream itself
             ;; Echo streams have data = (cons input output . unread-or-nil)
             ;; Actually keep it simple: delegate to input stream
             (let ((ch (%read-char-from-stream (car data) eof-error-p eof-value)))
               (when (characterp ch)
                 (%write-char-to-stream ch (cdr data)))
               ch)))
          ;; Two-way stream: read from input side
          ((= ty 4)
           (%read-char-from-stream (car (%stream-data s)) eof-error-p eof-value))
          ;; Concatenated stream: read from first non-exhausted stream
          ((= ty 6)
           (let ((data (%stream-data s)))
             (let ((streams (car data)))
               (loop
                 (when (null streams)
                   (return (if eof-error-p nil eof-value)))
                 (let ((ch (%read-char-from-stream (car streams) nil :eof-sentinel-7770002)))
                   (if (eq ch :eof-sentinel-7770002)
                       (progn
                         (setq streams (cdr streams))
                         (set-car data streams))
                       (return ch)))))))
          ;; File stream
          ((= ty 9)
           (%fs-read-char s eof-error-p eof-value))
          ;; Serial-io
          ((= ty 8) (if eof-error-p nil eof-value))
          (t (if eof-error-p nil eof-value))))))

;;; --- unread-char ---

(defun unread-char (ch &rest args)
  "Push back a character onto a stream."
  (let ((stream-arg (if args (car args) nil)))
    (let ((s (%resolve-input-stream stream-arg)))
      (when (streamp s)
        (let ((ty (%stream-type s)))
          (cond
            ((= ty 1)
             ;; String-input: store in unread slot
             (let ((pos-cell (cdr (%stream-data s))))
               (set-cdr pos-cell ch)))
            ((= ty 3)
             ;; Echo stream: unread on input side (don't echo unreads)
             (unread-char ch (car (%stream-data s))))
            ((= ty 4)
             ;; Two-way: unread on input side
             (unread-char ch (car (%stream-data s))))
            ((= ty 9)
             ;; File stream: push back by decrementing bpos
             (let ((bpos (%fs-bpos s)))
               (when (> bpos 0)
                 (%fs-set-bpos s (- bpos 1)))))))
      nil))))

;;; --- peek-char ---

(defun peek-char (&rest args)
  "Peek at next character. peek-type: nil=next char, t=skip whitespace, char=skip until char."
  (let ((peek-type (if args (car args) nil))
        (stream-arg (if (cdr args) (cadr args) nil))
        (eof-error-p (if (cddr args) (caddr args) t))
        (eof-value (if (cdddr args) (cadddr args) nil)))
    (let ((s (%resolve-input-stream stream-arg)))
      (cond
        ;; nil: just peek at next char
        ((null peek-type)
         (let ((ch (%read-char-from-stream s eof-error-p eof-value)))
           (when (characterp ch)
             (unread-char ch s))
           ch))
        ;; t: skip whitespace, peek at first non-whitespace
        ((eq peek-type t)
         (loop
           (let ((ch (%read-char-from-stream s eof-error-p eof-value)))
             (cond
               ((not (characterp ch)) (return ch))
               ((not (%whitespace-p ch))
                (unread-char ch s)
                (return ch))))))
        ;; character: skip until that character
        ((characterp peek-type)
         (loop
           (let ((ch (%read-char-from-stream s eof-error-p eof-value)))
             (cond
               ((not (characterp ch)) (return ch))
               ((char= ch peek-type)
                (unread-char ch s)
                (return ch))))))
        (t nil)))))

(defun %whitespace-p (ch)
  "Check if character is whitespace."
  (let ((code (char-code ch)))
    (or (= code 32) (= code 10) (= code 13) (= code 9) (= code 12))))

;;; --- read-char-no-hang ---

(defun read-char-no-hang (&rest args)
  "Non-blocking read-char. For string streams, same as read-char."
  (let ((stream-arg (if args (car args) nil))
        (eof-error-p (if (cdr args) (cadr args) t))
        (eof-value (if (cddr args) (caddr args) nil)))
    (let ((s (%resolve-input-stream stream-arg)))
      (if (and (streamp s) (= (%stream-type s) 1))
          (%read-char-from-stream s eof-error-p eof-value)
          nil))))

;;; --- Core write-char ---

(defun %write-char-to-stream (code stream)
  "Write a char code (integer) to a resolved stream. Caller must convert characters first."
  (if (not (streamp stream))
      (write-char-serial code)
      (let ((ty (%stream-type stream)))
        (cond
          ;; String-output: collect char codes
          ((= ty 2)
           (let ((data (%stream-data stream)))
             (set-car data (cons code (car data)))))
          ;; Echo stream: write to output side
          ((= ty 3)
           (%write-char-to-stream code (cdr (%stream-data stream))))
          ;; Two-way stream: write to output side
          ((= ty 4)
           (%write-char-to-stream code (cdr (%stream-data stream))))
          ;; Broadcast: write to all
          ((= ty 5)
           (dolist (s (%stream-data stream))
             (%write-char-to-stream code s)))
          ;; File stream
          ((= ty 9)
           (%fs-write-char code stream))
          ;; Serial-io
          ((= ty 8) (write-char-serial code))
          ;; Synonym: resolve target symbol's value and delegate.
          ((= ty 7)
           (let ((target (%stream-data stream)))
             (let ((target-stream (cond ((symbolp target) (symbol-value target))
                                        ((stringp target) (symbol-value (intern target)))
                                        (t target))))
               (if (streamp target-stream)
                   (%write-char-to-stream code target-stream)
                   (write-char-serial code)))))
          (t (write-char-serial code))))))

;; Backward-compatible wrapper used by write-to-stream, princ-to-stream etc.
(defun write-char-to-stream (ch stream)
  (let ((code (%ensure-char-code ch)))
    (if (null stream)
        (write-char-serial code)
        (if (streamp stream)
            (%write-char-to-stream code stream)
            ;; Legacy: old-style cons output stream (char-list . nil)
            (set-car stream (cons code (car stream)))))))

