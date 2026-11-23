;;;; make-ape.lisp -- one file that runs on Windows and on Linux: an Actually
;;;; Portable Executable in its "fat" form.
;;;;
;;;;   MODUS_APE_PE=modus-windows-x64.exe MODUS_APE_ELF=modus-linux-x64 \
;;;;   MODUS_APE_OUT=modus.com  modus --script scripts/make-ape.lisp   # or sbcl --script
;;;;
;;;; The output is the Windows image (mvm/build-win64-cli.lisp) with the Linux image
;;;; (mvm/build-generic-cli.lisp) appended, 4 KB aligned, as PE overlay data, which the
;;;; Windows loader ignores.  Its first bytes are Cosmopolitan's APE magic,
;;;; "MZqFpD='" and a newline: "MZ" for Windows, and for a Unix shell -- which runs the
;;;; file as a script when the kernel answers ENOEXEC -- the start of a quoted string
;;;; that swallows the rest of the DOS header (e_lfanew included).  The quote closes in
;;;; the DOS stub, and a few lines of sh copy the ELF to ~/.cache/modus/ once (named
;;;; after its contents, so a new release never runs a stale copy) and exec it.  The
;;;; copy is the ELF's exact bytes, so its SHA-256 is the release's modus-linux-x64:
;;;; what runs on Unix is the cross-checked image, not something derived from it.  The PE
;;;; header moves from 0x80 to 0x280 to make room; nothing else in the PE changes.
;;;;
;;;; Not the single-image APE Cosmopolitan builds: the two images differ (syscalls go
;;;; through a shim on Windows; per-thread storage is TEB slots there, FS/GS on Linux),
;;;; so the file carries both.  The script is POSIX sh (bash, dash, zsh, busybox ash).

(defun ape-env (name &optional default)
  (let ((v (funcall (find-symbol "POSIX-GETENV" "SB-EXT") name)))   ; SBCL's, and modus's
    (if (and v (plusp (length v))) v
        (or default (error "make-ape: set ~A" name)))))

(defun ape-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s)
      b)))

(defun ape-u16 (b i) (logior (aref b i) (ash (aref b (+ i 1)) 8)))
(defun ape-u32 (b i) (logior (ape-u16 b i) (ash (ape-u16 b (+ i 2)) 16)))
(defun ape-put-u32 (b i v)
  (dotimes (k 4) (setf (aref b (+ i k)) (ldb (byte 8 (* 8 k)) v))))

(defun ape-fnv64 (b)
  "FNV-1a, 64 bits, of the octets B, as 16 hex digits: the cache file's name."
  (let ((h #xcbf29ce484222325))
    (dotimes (i (length b))
      (setq h (logand (* (logxor h (aref b i)) #x100000001b3) #xFFFFFFFFFFFFFFFF)))
    (format nil "~(~16,'0X~)" h)))

(defparameter +ape-pe-at+ #x280
  "Where the PE header goes.  It must end within SizeOfHeaders (0x400 here), and
   e_lfanew's bytes must hold no quote: #x80 #x02 #x00 #x00.")

(defun ape-segments-from-env ()
  "MODUS_APE_SEGMENTS: \"NAME=PATH NAME=PATH ...\", the extra segments, in order."
  (let ((spec (ape-env "MODUS_APE_SEGMENTS" " ")) (acc nil) (start 0))
    (loop
      (let* ((sp (position #\Space spec :start start))
             (word (subseq spec start sp)))
        (when (plusp (length word))
          (let ((eq (position #\= word)))
            (unless eq (error "make-ape: MODUS_APE_SEGMENTS entry ~S is not NAME=PATH" word))
            (push (cons (subseq word 0 eq) (ape-read (subseq word (1+ eq)))) acc)))
        (if sp (setq start (1+ sp)) (return))))
    (nreverse acc)))

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

(defun make-ape (pe elf)
  (let* ((lfanew (ape-u32 pe #x3C))
         (nsec (ape-u16 pe (+ lfanew 6)))
         (optsz (ape-u16 pe (+ lfanew 20)))
         (hdr-end (+ lfanew 24 optsz (* 40 nsec)))
         (hdr-len (- hdr-end lfanew))
         (size-of-headers (ape-u32 pe (+ lfanew 24 60)))
         (first-raw (loop for i below nsec
                          for raw = (ape-u32 pe (+ lfanew 24 optsz (* 40 i) 20))
                          when (plusp raw) minimize raw))
         (elf-at (* 4096 (ceiling (length pe) 4096)))
         (elf-blocks (ceiling (length elf) 4096))
         (name (concatenate 'string "modus-" (ape-fnv64 elf)))
         (script (format nil "~C'~%~
case $(uname -sm) in \"Linux x86_64\") ;; *) echo \"modus.com: no image for $(uname -sm) in this file\" >&2; exit 1;; esac~%~
o=$0; case $o in */*) ;; *) o=$(command -v \"$o\");; esac; case $o in /*) ;; *) o=$PWD/$o;; esac~%~
d=${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/modus; e=$d/~A~%~
[ -x \"$e\" ] || { mkdir -p \"$d\" && dd if=\"$o\" bs=4096 skip=~D count=~D 2>/dev/null | head -c ~D > \"$e.$$\" ~
&& chmod 755 \"$e.$$\" && mv -f \"$e.$$\" \"$e\"; } || exit 1~%~
MODUS_APE=$o exec \"$e\" \"$@\"~%exit 1~%"
                         #\Newline name (/ elf-at 4096) elf-blocks (length elf)))
         (out (make-array (+ elf-at (* 4096 elf-blocks))
                          :element-type '(unsigned-byte 8) :initial-element 0)))
    (unless (and (= (aref pe 0) 77) (= (aref pe 1) 90) (= (ape-u32 pe lfanew) #x4550))
      (error "make-ape: ~A is not a PE image" "MODUS_APE_PE"))
    (unless (and (= (aref elf 0) #x7F) (= (aref elf 1) 69) (= (aref elf 2) 76) (= (aref elf 3) 70))
      (error "make-ape: MODUS_APE_ELF is not an ELF image"))
    (when (> (+ +ape-pe-at+ hdr-len) size-of-headers)
      (error "make-ape: the PE header does not fit below SizeOfHeaders (#x~X)" size-of-headers))
    (when (> (+ #x40 (length script)) +ape-pe-at+)
      (error "make-ape: the script (~D bytes) does not fit in the DOS stub" (length script)))
    (when (find (char-code #\') pe :end hdr-end :start 8)
      (error "make-ape: the PE header holds a quote byte"))
    ;; the PE, every byte, then the header moved
    (replace out pe)
    (fill out 0 :start 2 :end size-of-headers)
    (replace out pe :start1 +ape-pe-at+ :start2 lfanew :end2 hdr-end)
    (ape-put-u32 out #x3C +ape-pe-at+)
    ;; the APE magic: "MZqFpD='" and a newline, the first line a shell reads
    (let ((magic "MZqFpD='"))
      (dotimes (i (length magic)) (setf (aref out i) (char-code (char magic i)))))
    (setf (aref out 8) 10)
    (dotimes (i (length script))
      (setf (aref out (+ #x40 i)) (char-code (char script i))))
    (assert (<= size-of-headers first-raw))
    (replace out elf :start1 elf-at)
    (values out name elf-at)))

(let ((pe (ape-read (ape-env "MODUS_APE_PE")))
      (elf (ape-read (ape-env "MODUS_APE_ELF")))
      (path (ape-env "MODUS_APE_OUT" "modus.com")))
  (multiple-value-bind (out0 name elf-at) (make-ape pe elf)
    (let ((out (ape-trailer out0 (ape-segments-from-env)
                            (list (list "linux-x64.elf" elf-at (length elf))))))
      (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence out s))
      (format t "~&make-ape: ~A, ~D bytes (Linux image cached as ~~/.cache/modus/~A)~%"
              path (length out) name))))
