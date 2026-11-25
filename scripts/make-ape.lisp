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
;;;; MODUS_APE_MACHO adds the macOS/arm64 CLI (mvm/build-macos-cli.lisp) after the Linux
;;;; image; the script picks by `uname -sm' ("Linux x86_64", "Darwin arm64").  With
;;;; MODUS_APE_CRAM both go in gzip-compressed, and with MODUS_APE_STUB too the PE is the
;;;; Windows launcher (mvm/ape-stub-win64.lisp) and the Windows CLI a compressed segment.
;;;; The script must fit in the DOS stub (0x40..0x280), which is why it is terse.
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

;; APE-TRAILER is in ape-segments.lisp, beside this file: kiln's bundle
;; (kiln/boot/bundle.lisp) appends its segments with it too.
(load (merge-pathnames "ape-segments.lisp" (or *load-truename* *default-pathname-defaults*)))

(defun ape-gzip (octets)
  "OCTETS gzip-compressed by cram (MODUS_APE_CRAM: its source in one file -- the cram.lisp
   segment), with mtime 0: the same bytes on every host."
  (let ((cram (ape-env "MODUS_APE_CRAM")))
    (unless (find-package "CRAM") (load cram))
    #+modus (funcall (find-symbol "JIT-EAGER" "MODUS"))   ; one long call: native first
    (funcall (find-symbol "GZIP-COMPRESS" "CRAM") octets :mtime 0)))

(defun make-ape (pe images)
  "IMAGES: one plist per Unix platform, in file order --
     :UNAME  what `uname -sm' prints there (\"Linux x86_64\", \"Darwin arm64\")
     :RAW    the executable's bytes (what the cache holds, and what is checked)
     :STORED what the file carries: RAW, or RAW gzip-compressed (the script then runs
             it through gzip -dc)
   Each is stored 4 KB aligned after the PE.  Returns the file, the cache names, and each
   image's offset."
  (let* ((lfanew (ape-u32 pe #x3C))
         (nsec (ape-u16 pe (+ lfanew 6)))
         (optsz (ape-u16 pe (+ lfanew 20)))
         (hdr-end (+ lfanew 24 optsz (* 40 nsec)))
         (hdr-len (- hdr-end lfanew))
         (size-of-headers (ape-u32 pe (+ lfanew 24 60)))
         (first-raw (loop for i below nsec
                          for raw = (ape-u32 pe (+ lfanew 24 optsz (* 40 i) 20))
                          when (plusp raw) minimize raw))
         (at (* 4096 (ceiling (length pe) 4096)))
         (offsets (loop for im in images
                        collect at
                        do (incf at (* 4096 (ceiling (length (getf im :stored)) 4096)))))
         (names (loop for im in images
                      collect (concatenate 'string "modus-" (ape-fnv64 (getf im :raw)))))
         ;; one arm per platform sets s (first 4 KB block), c (blocks), l (stored
         ;; length) and n (the cache name); one line then copies the image to the cache
         ;; once and runs it.  The script must fit in the DOS stub, so when every image
         ;; is gzip'd its check is gzip's own (CRC-32 and length: a short or damaged copy
         ;; exits non-zero, which stops the && chain); otherwise u (the length) and z
         ;; (gzip -dc or cat) are set too and the copy's length is checked.
         (gz (every (lambda (im) (not (eq (getf im :stored) (getf im :raw)))) images))
         (arms (with-output-to-string (o)
                 (loop for im in images for off in offsets for n in names
                       for stored = (getf im :stored)
                       do (format o "\"~A\")s=~D c=~D l=~D n=~A~@[ u=~D z=~A~];;"
                                  (getf im :uname) (/ off 4096)
                                  (ceiling (length stored) 4096) (length stored) n
                                  (and (not gz) (length (getf im :raw)))
                                  (if (eq stored (getf im :raw)) "cat" "'gzip -dc'")))))
         (script (format nil "~C'~%~
case $(uname -sm) in ~A*)echo \"modus.com: no image for $(uname -sm)\" >&2;exit 1;;esac~%~
o=$0;case $o in */*);; *)o=$(command -v \"$o\");;esac;case $o in /*);; *)o=$PWD/$o;;esac~%~
d=${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/modus;e=$d/$n~%~
[ -x \"$e\" ]||{ mkdir -p \"$d\"&&dd if=\"$o\" bs=4096 skip=$s count=$c 2>/dev/null|head -c $l|~A>\"$e.$$\"~
~A&&chmod +x \"$e.$$\"&&mv -f \"$e.$$\" \"$e\";}||{ rm -f \"$e.$$\";exit 1;}~%~
MODUS_APE=$o exec \"$e\" \"$@\"~%exit 1~%"
                         #\Newline arms (if gz "gzip -dc" "$z")
                         (if gz "" "&&[ $(wc -c<\"$e.$$\") -eq $u ]")))
         (out (make-array at :element-type '(unsigned-byte 8) :initial-element 0)))
    (unless (and (= (aref pe 0) 77) (= (aref pe 1) 90) (= (ape-u32 pe lfanew) #x4550))
      (error "make-ape: ~A is not a PE image" "MODUS_APE_PE"))
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
    (loop for im in images for off in offsets
          do (replace out (getf im :stored) :start1 off))
    (values out names offsets (length script))))

(defun ape-opt (name)
  (let ((v (funcall (find-symbol "POSIX-GETENV" "SB-EXT") name))) (and v (plusp (length v)) v)))

(let* ((pe (ape-read (ape-env "MODUS_APE_PE")))
       (elf (ape-read (ape-env "MODUS_APE_ELF")))
       (path (ape-env "MODUS_APE_OUT" "modus.com"))
       ;; MODUS_APE_CRAM (cram's source): the Linux image goes in gzip-compressed
       (elf-z (and (ape-opt "MODUS_APE_CRAM") (ape-gzip elf)))
       ;; MODUS_APE_STUB (mvm/build-ape-stub.lisp's launcher), with MODUS_APE_CRAM: the file's
       ;; PE is the launcher, and the Windows CLI goes in gzip-compressed as the segment
       ;; windows-x64.exe.gz, which the launcher inflates, caches and runs
       (stub (and (ape-opt "MODUS_APE_STUB") (ape-read (ape-opt "MODUS_APE_STUB"))))
       (pe-z (and stub (or elf-z (error "make-ape: MODUS_APE_STUB needs MODUS_APE_CRAM"))
                  (ape-gzip pe))))
  (unless (and (= (aref elf 0) #x7F) (= (aref elf 1) 69) (= (aref elf 2) 76) (= (aref elf 3) 70))
    (error "make-ape: MODUS_APE_ELF is not an ELF image"))
  ;; MODUS_APE_MACHO (mvm/build-macos-cli.lisp's executable): the macOS/arm64 CLI, stored
  ;; after the Linux image (gzip-compressed with MODUS_APE_CRAM); Darwin's sh runs it the
  ;; same way
  (let* ((macho (and (ape-opt "MODUS_APE_MACHO") (ape-read (ape-opt "MODUS_APE_MACHO"))))
         (macho-z (and macho elf-z (ape-gzip macho)))
         (images (append (list (list :uname "Linux x86_64" :raw elf :stored (or elf-z elf)
                                     :dir (if elf-z "linux-x64.elf.gz" "linux-x64.elf")))
                         (and macho
                              (list (list :uname "Darwin arm64" :raw macho
                                          :stored (or macho-z macho)
                                          :dir (if macho-z "darwin-arm64.gz" "darwin-arm64")))))))
    (when (and macho (not (= (ape-u32 macho 0) #xFEEDFACF)))
      (error "make-ape: MODUS_APE_MACHO is not a 64-bit Mach-O"))
    (multiple-value-bind (out0 names offsets script-len) (make-ape (or stub pe) images)
      (let ((out (ape-trailer out0 (append (and pe-z (list (cons "windows-x64.exe.gz" pe-z)))
                                           (ape-segments-from-env))
                              (loop for im in images for off in offsets
                                    collect (list (getf im :dir) off (length (getf im :stored)))))))
        (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          (write-sequence out s))
        (format t "~&make-ape: ~A, ~D bytes, a ~D-byte script; cached in ~~/.cache/modus/ as ~{~A~^, ~}~%"
                path (length out) script-len names)))))
