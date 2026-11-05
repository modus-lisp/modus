;;;; test/cabinet-bulk-write.lisp -- WRITE-SEQUENCE / WRITE-STRING onto a cabinet file go in one
;;;; %CAB call, and say the same bytes the per-byte path says.
;;;;
;;;;   ./modus --script test/cabinet-bulk-write.lisp      -> CABBULK PASS / FAIL / SKIP
;;;;
;;;; Needs pagetree and cabinet checkouts (MODUS_LISP, default ~/modus-lisp).  Byte by byte, each
;;;; byte was a dispatch, three fd-table walks and an EQUAL walk of the shim's write cache: on a
;;;; QEMU board 58 us a byte, and the gateway's boot spent 41 s writing its CA bundle (WRITE-STRING)
;;;; and client payload (WRITE-SEQUENCE).  Checks, each against the per-byte path's own output:
;;;; byte vectors, a :START/:END slice, strings with 2-, 3- and 4-byte UTF-8, an append, and a shim
;;;; without :WRITE-BYTES (an older lib/cabinet-fs.lisp) falling back byte by byte.

(defvar *cb-root* (or (sb-ext:posix-getenv "MODUS_LISP")
                      (namestring (merge-pathnames "modus-lisp/" (user-homedir-pathname)))))
(defvar *cb-fail* 0)
(defun cb-check (what got want)
  (format t "~&  ~a ~a~%" (if (equalp got want) "ok  " "FAIL") what)
  (unless (equalp got want)
    (format t "       got  ~s~%       want ~s~%" got want)
    (incf *cb-fail*)))

(unless (probe-file (format nil "~acabinet/src/fs.lisp" *cb-root*))
  (format t "~&CABBULK SKIP (no cabinet checkout under ~a)~%" *cb-root*)
  (sb-ext:exit :code 0))

(dolist (f '("pagetree/src/packages" "pagetree/src/device" "pagetree/src/device-file"
             "pagetree/src/device-sector" "pagetree/src/pager" "pagetree/src/node"
             "pagetree/src/btree" "pagetree/src/store"
             "cabinet/src/packages" "cabinet/src/utf8" "cabinet/src/codec" "cabinet/src/fs"))
  (load (format nil "~a~a.lisp" *cb-root* f)))
(load (merge-pathnames "../lib/cabinet-fs.lisp" *load-truename*))
(modus-int::cabinet-mount)

(defun cb-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))
(defun cb-per-byte (path seq)
  "The reference: the same bytes through WRITE-BYTE, one %CAB call each."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede :if-does-not-exist :create)
    (loop for b across seq do (write-byte b s))))
(defun cb-calls (thunk)
  "How many %CAB calls THUNK makes, and its value."
  (let* ((n 0) (inner modus-int::*cab-call*))
    (setq modus-int::*cab-call* (lambda (op &rest args) (incf n) (apply inner op args)))
    (unwind-protect (progn (funcall thunk) n)
      (setq modus-int::*cab-call* inner))))

(let* ((n 70000)
       (v (let ((a (make-array n :element-type '(unsigned-byte 8))))
            (dotimes (i n a) (setf (aref a i) (mod (* i 7) 251))))))
  (cb-per-byte "/t/ref.bin" v)
  (let ((calls (cb-calls (lambda ()
                           (with-open-file (s "/t/bulk.bin" :direction :output
                                                            :element-type '(unsigned-byte 8)
                                                            :if-exists :supersede
                                                            :if-does-not-exist :create)
                             (write-sequence v s))))))
    (cb-check "write-sequence: same bytes as write-byte" (cb-bytes "/t/bulk.bin") (cb-bytes "/t/ref.bin"))
    (cb-check "write-sequence: under 10 %CAB calls for 70000 bytes" (< calls 10) t))
  ;; a slice, then a second write-sequence appending to it
  (with-open-file (s "/t/slice.bin" :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :supersede :if-does-not-exist :create)
    (write-sequence v s :start 100 :end 300)
    (write-byte 7 s)
    (write-sequence v s :end 50))
  (cb-check "slice + byte + slice, in order"
            (cb-bytes "/t/slice.bin")
            (concatenate '(vector (unsigned-byte 8)) (subseq v 100 300) #(7) (subseq v 0 50)))
  (with-open-file (s "/t/slice.bin" :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :append)
    (write-sequence v s :start 10 :end 20))
  (cb-check "append after the slices"
            (cb-bytes "/t/slice.bin")
            (concatenate '(vector (unsigned-byte 8))
                         (subseq v 100 300) #(7) (subseq v 0 50) (subseq v 10 20))))

(let ((text (format nil "PEM ~a caf~a ~a ~a end~%line 2~%"
                    (make-string 3000 :initial-element #\A)
                    (code-char #xE9) (code-char #x20AC) (code-char #x1F600))))
  (with-open-file (s "/t/ref.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for c across text do (write-char c s)))
  (let ((calls (cb-calls (lambda ()
                           (with-open-file (s "/t/bulk.txt" :direction :output
                                                            :if-exists :supersede
                                                            :if-does-not-exist :create)
                             (write-string text s))))))
    (cb-check "write-string: same UTF-8 bytes as write-char" (cb-bytes "/t/bulk.txt") (cb-bytes "/t/ref.txt"))
    (cb-check "write-string: under 10 %CAB calls" (< calls 10) t))
  (with-open-file (s "/t/bulk.txt") (cb-check "write-string: reads back as the string"
                                              (let ((r (make-string (length text))))
                                                (subseq r 0 (read-sequence r s)))
                                              text))
  (with-open-file (s "/t/mix.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text s :start 2 :end 9)
    (write-char #\| s)
    (write-string text s :start 3003 :end 3010))
  (cb-check "write-string :start/:end, then write-char, in order"
            (with-open-file (s "/t/mix.txt") (read-line s))
            (concatenate 'string (subseq text 2 9) "|" (subseq text 3003 3010))))

;; An older shim: no :WRITE-BYTES.  The kernel must fall back, not lose the write.
(let* ((inner modus-int::*cab-call*)
       (v (coerce #(1 2 3 4 5 250) '(vector (unsigned-byte 8)))))
  (setq modus-int::*cab-call*
        (lambda (op &rest args)
          (if (eq op :write-bytes) (error "cabinet-fs: unknown op") (apply inner op args))))
  (unwind-protect
       (progn
         (with-open-file (s "/t/old.bin" :direction :output :element-type '(unsigned-byte 8)
                                         :if-exists :supersede :if-does-not-exist :create)
           (write-sequence v s))
         (with-open-file (s "/t/old.txt" :direction :output :if-exists :supersede
                                         :if-does-not-exist :create)
           (write-string "old shim" s)))
    (setq modus-int::*cab-call* inner))
  (cb-check "old shim: write-sequence falls back byte by byte" (cb-bytes "/t/old.bin") v)
  (cb-check "old shim: write-string falls back byte by byte"
            (with-open-file (s "/t/old.txt") (read-line s)) "old shim"))

(format t "~&CABBULK ~a~%" (if (zerop *cb-fail*) "PASS" "FAIL"))
