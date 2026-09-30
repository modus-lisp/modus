;;; net/sb-gray-shim.lisp -- Gray streams for modus, installed ON DEMAND.
;;;
;;; Baked into the hosted image as a source string (mvm/build-cli-common.lisp,
;;; %SB-GRAY-SOURCE) but NOT evaluated at boot: evaluating it there took every
;;; script's startup from ~4.1 s to ~7.7 s and pushed the web image's
;;; interpreted boot past the gate's budget.  %ENSURE-GRAY-STREAMS evaluates it
;;; once; ASDF:LOAD-SYSTEM calls that before loading anything, which is how
;;; every library that subclasses a FUNDAMENTAL-* class (seal, flexi-streams,
;;; chunga) arrives.  A program defining a Gray stream without ASDF calls
;;; (%ENSURE-GRAY-STREAMS) itself.

;;; ============================================================
;;; GRAY STREAMS
;;; ============================================================
;;;
;;; The GRAY-STREAMS package (net/genera-compat.lisp) used to hold symbols
;;; only: no classes, and no CL stream function dispatched to a user stream,
;;; so a library stream class (seal's TLS-STREAM, flexi-streams, chunga) was
;;; a CLOS object READ-BYTE rejected as "not a stream".  This is the Gray
;;; proposal: the FUNDAMENTAL-* classes, the generic functions with the
;;; proposal's default methods, and the CL entry points wrapped so an
;;; instance of FUNDAMENTAL-STREAM goes to its generic and anything else to
;;; the original function.  The wrappers are installed through the function
;;; cell, which is what runtime-compiled callers (interpreted and JIT) reach.
;;; Character OUTPUT from FORMAT/PRINC/etc. reaches a Gray stream through
;;; *GRAY-ROOT-CLASS* / *GRAY-WRITE-CHAR-FN*, which mvm/cl-fileio.lisp's
;;; %WRITE-CHAR-TO-STREAM consults.

(defclass gray-streams:fundamental-stream ()
  ((%gray-open :initform t :accessor %gray-open-p)))
(defclass gray-streams:fundamental-input-stream (gray-streams:fundamental-stream) ())
(defclass gray-streams:fundamental-output-stream (gray-streams:fundamental-stream) ())
(defclass gray-streams:fundamental-character-stream (gray-streams:fundamental-stream) ())
(defclass gray-streams:fundamental-binary-stream (gray-streams:fundamental-stream) ())
(defclass gray-streams:fundamental-character-input-stream
    (gray-streams:fundamental-input-stream gray-streams:fundamental-character-stream) ())
(defclass gray-streams:fundamental-character-output-stream
    (gray-streams:fundamental-output-stream gray-streams:fundamental-character-stream) ())
(defclass gray-streams:fundamental-binary-input-stream
    (gray-streams:fundamental-input-stream gray-streams:fundamental-binary-stream) ())
(defclass gray-streams:fundamental-binary-output-stream
    (gray-streams:fundamental-output-stream gray-streams:fundamental-binary-stream) ())

(defun %gray-p (x)
  (and x (not (eq x t)) (typep x 'gray-streams:fundamental-stream)))

;;; --- generics, with the proposal's defaults ---
(defgeneric gray-streams:stream-read-byte (stream))
(defgeneric gray-streams:stream-write-byte (stream integer))
(defgeneric gray-streams:stream-read-char (stream))
(defgeneric gray-streams:stream-unread-char (stream character))
(defgeneric gray-streams:stream-read-char-no-hang (stream))
(defmethod gray-streams:stream-read-char-no-hang ((s gray-streams:fundamental-stream))
  (gray-streams:stream-read-char s))
(defgeneric gray-streams:stream-peek-char (stream))
(defmethod gray-streams:stream-peek-char ((s gray-streams:fundamental-stream))
  (let ((c (gray-streams:stream-read-char s)))
    (unless (eq c :eof) (gray-streams:stream-unread-char s c))
    c))
(defgeneric gray-streams:stream-listen (stream))
(defmethod gray-streams:stream-listen ((s gray-streams:fundamental-stream)) t)
(defgeneric gray-streams:stream-read-line (stream))
(defmethod gray-streams:stream-read-line ((s gray-streams:fundamental-stream))
  (let ((acc nil))
    (loop
      (let ((c (gray-streams:stream-read-char s)))
        (cond ((eq c :eof)
               (return (values (coerce (nreverse acc) 'string) t)))
              ((char= c #\Newline)
               (return (values (coerce (nreverse acc) 'string) nil)))
              (t (push c acc)))))))
(defgeneric gray-streams:stream-clear-input (stream))
(defmethod gray-streams:stream-clear-input ((s gray-streams:fundamental-stream)) nil)
(defgeneric gray-streams:stream-write-char (stream character))
(defgeneric gray-streams:stream-line-column (stream))
(defmethod gray-streams:stream-line-column ((s gray-streams:fundamental-stream)) nil)
(defgeneric gray-streams:stream-start-line-p (stream))
(defmethod gray-streams:stream-start-line-p ((s gray-streams:fundamental-stream))
  (eql (gray-streams:stream-line-column s) 0))
(defgeneric gray-streams:stream-write-string (stream string &optional start end))
(defmethod gray-streams:stream-write-string ((s gray-streams:fundamental-stream) string
                                             &optional (start 0) end)
  (let ((end (or end (length string))))
    (do ((i start (+ i 1))) ((>= i end) string)
      (gray-streams:stream-write-char s (char string i)))))
(defgeneric gray-streams:stream-terpri (stream))
(defmethod gray-streams:stream-terpri ((s gray-streams:fundamental-stream))
  (gray-streams:stream-write-char s #\Newline) nil)
(defgeneric gray-streams:stream-fresh-line (stream))
(defmethod gray-streams:stream-fresh-line ((s gray-streams:fundamental-stream))
  (unless (gray-streams:stream-start-line-p s)
    (gray-streams:stream-terpri s) t))
(defgeneric gray-streams:stream-finish-output (stream))
(defmethod gray-streams:stream-finish-output ((s gray-streams:fundamental-stream))
  (gray-streams:stream-force-output s))
(defgeneric gray-streams:stream-force-output (stream))
(defmethod gray-streams:stream-force-output ((s gray-streams:fundamental-stream)) nil)
(defgeneric gray-streams:stream-clear-output (stream))
(defmethod gray-streams:stream-clear-output ((s gray-streams:fundamental-stream)) nil)
(defgeneric gray-streams:stream-advance-to-column (stream column))
(defmethod gray-streams:stream-advance-to-column ((s gray-streams:fundamental-stream) column)
  (let ((c (gray-streams:stream-line-column s)))
    (when c
      (dotimes (i (- column c)) (gray-streams:stream-write-char s #\Space))
      t)))
(defgeneric gray-streams:stream-read-sequence (stream seq &optional start end))
(defmethod gray-streams:stream-read-sequence ((s gray-streams:fundamental-stream) seq
                                              &optional (start 0) end)
  (let ((end (or end (length seq)))
        (binp (typep s 'gray-streams:fundamental-binary-stream)))
    (do ((i start (+ i 1))) ((>= i end) end)
      (let ((x (if binp (gray-streams:stream-read-byte s) (gray-streams:stream-read-char s))))
        (when (eq x :eof) (return i))
        (setf (elt seq i) x)))))
(defgeneric gray-streams:stream-write-sequence (stream seq &optional start end))
(defmethod gray-streams:stream-write-sequence ((s gray-streams:fundamental-stream) seq
                                               &optional (start 0) end)
  (let ((end (or end (length seq)))
        (binp (typep s 'gray-streams:fundamental-binary-stream)))
    (do ((i start (+ i 1))) ((>= i end) seq)
      (if binp
          (gray-streams:stream-write-byte s (elt seq i))
          (gray-streams:stream-write-char s (elt seq i))))))
(defgeneric gray-streams:stream-file-position (stream))
(defmethod gray-streams:stream-file-position ((s gray-streams:fundamental-stream)) nil)

;;; --- the CL entry points ---
(defmacro %gray-wrap (name lambda-list gray-form)
  "Install NAME := a function that runs GRAY-FORM when its stream argument
   (bound to STREAM in LAMBDA-LIST's sense by GRAY-FORM itself) is a Gray
   stream, and otherwise applies the ORIGINAL definition to its arguments."
  `(let ((orig (symbol-function ',name)))
     (setf (symbol-function ',name)
           (lambda (&rest args)
             (destructuring-bind ,lambda-list args
               (declare (ignorable ,@(remove-if (lambda (x) (member x '(&optional &rest &key)))
                                                (mapcar (lambda (x) (if (consp x) (car x) x)) lambda-list))))
               (if (%gray-p stream) ,gray-form (apply orig args)))))))

(defun %gray-eof (stream eof-error-p eof-value)
  (if eof-error-p (error 'end-of-file :stream stream) eof-value))

(%gray-wrap read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((b (gray-streams:stream-read-byte stream)))
    (if (eq b :eof) (%gray-eof stream eof-error-p eof-value) b)))
(%gray-wrap write-byte (byte stream)
  (progn (gray-streams:stream-write-byte stream byte) byte))
(%gray-wrap read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (let ((c (gray-streams:stream-read-char stream)))
    (if (eq c :eof) (%gray-eof stream eof-error-p eof-value) c)))
(%gray-wrap read-char-no-hang (&optional stream (eof-error-p t) eof-value recursive-p)
  (let ((c (gray-streams:stream-read-char-no-hang stream)))
    (if (eq c :eof) (%gray-eof stream eof-error-p eof-value) c)))
(%gray-wrap unread-char (character &optional stream)
  (progn (gray-streams:stream-unread-char stream character) nil))
(%gray-wrap peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  (let ((c (if (null peek-type)
               (gray-streams:stream-peek-char stream)
               (loop
                 (let ((c (gray-streams:stream-read-char stream)))
                   (when (or (eq c :eof)
                             (if (eq peek-type t)
                                 (not (member c '(#\Space #\Tab #\Newline #\Return #\Page)))
                                 (char= c peek-type)))
                     (unless (eq c :eof) (gray-streams:stream-unread-char stream c))
                     (return c)))))))
    (if (eq c :eof) (%gray-eof stream eof-error-p eof-value) c)))
(%gray-wrap read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  (multiple-value-bind (line missing) (gray-streams:stream-read-line stream)
    (if (and missing (= (length line) 0))
        (%gray-eof stream eof-error-p eof-value)
        (values line missing))))
(%gray-wrap listen (&optional stream) (gray-streams:stream-listen stream))
(%gray-wrap clear-input (&optional stream) (gray-streams:stream-clear-input stream))
(%gray-wrap write-char (character &optional stream)
  (progn (gray-streams:stream-write-char stream character) character))
(%gray-wrap write-string (string &optional stream &key (start 0) end)
  (progn (gray-streams:stream-write-string stream string start end) string))
(%gray-wrap write-line (string &optional stream &key (start 0) end)
  (progn (gray-streams:stream-write-string stream string start end)
         (gray-streams:stream-terpri stream) string))
(%gray-wrap terpri (&optional stream) (progn (gray-streams:stream-terpri stream) nil))
(%gray-wrap fresh-line (&optional stream) (gray-streams:stream-fresh-line stream))
(%gray-wrap finish-output (&optional stream) (progn (gray-streams:stream-finish-output stream) nil))
(%gray-wrap force-output (&optional stream) (progn (gray-streams:stream-force-output stream) nil))
(%gray-wrap clear-output (&optional stream) (progn (gray-streams:stream-clear-output stream) nil))
(%gray-wrap read-sequence (seq stream &key (start 0) end)
  (gray-streams:stream-read-sequence stream seq start end))
(%gray-wrap write-sequence (seq stream &key (start 0) end)
  (progn (gray-streams:stream-write-sequence stream seq start end) seq))
(%gray-wrap close (stream &key abort)
  (progn (setf (%gray-open-p stream) nil) t))
(%gray-wrap open-stream-p (stream) (%gray-open-p stream))
(%gray-wrap streamp (stream) t)
(%gray-wrap input-stream-p (stream) (typep stream 'gray-streams:fundamental-input-stream))
(%gray-wrap output-stream-p (stream) (typep stream 'gray-streams:fundamental-output-stream))
(%gray-wrap stream-element-type (stream)
  (if (typep stream 'gray-streams:fundamental-binary-stream) '(unsigned-byte 8) 'character))

;;; The printers resolve their destination inside the image, before any
;;; character is written, and reject what they do not recognise; for a Gray
;;; destination they render to a string with the original and hand it over.
(let ((orig (symbol-function 'format)))
  (setf (symbol-function 'format)
        (lambda (destination control &rest args)
          (if (%gray-p destination)
              (progn (gray-streams:stream-write-string
                      destination (apply orig nil control args))
                     nil)
              (apply orig destination control args)))))
(%gray-wrap princ (object &optional stream)
  (progn (gray-streams:stream-write-string stream (princ-to-string object)) object))
(%gray-wrap prin1 (object &optional stream)
  (progn (gray-streams:stream-write-string stream (prin1-to-string object)) object))
(%gray-wrap print (object &optional stream)
  (progn (gray-streams:stream-terpri stream)
         (gray-streams:stream-write-string stream (prin1-to-string object))
         (gray-streams:stream-write-char stream #\Space) object))
(%gray-wrap pprint (object &optional stream)
  (progn (gray-streams:stream-terpri stream)
         (gray-streams:stream-write-string stream (prin1-to-string object)) (values)))
(let ((orig (symbol-function 'write)))
  (setf (symbol-function 'write)
        (lambda (object &rest keys)
          (let ((stream (getf keys :stream)))
            (if (%gray-p stream)
                (let ((k (copy-list keys)))
                  (remf k :stream)
                  (gray-streams:stream-write-string
                   stream (apply (function write-to-string) object k))
                  object)
                (apply orig object keys))))))

;;; FORMAT / PRINC / WRITE-TO-STREAM reach character output through
;;; %WRITE-CHAR-TO-STREAM, compiled into the image; it consults these.
(setq *gray-root-class* 'gray-streams:fundamental-stream)
(setq *gray-write-char-fn* (lambda (stream code)
                             (gray-streams:stream-write-char stream (code-char code))))
