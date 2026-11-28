;;;; test/gray-stream-types.lisp -- a Gray stream is a STREAM and none of CL's stream subtypes.
;;;;
;;;;   ./modus --script test/gray-stream-types.lisp        (sbcl --script runs it too)
;;;;
;;;; STREAMP answers T for a Gray stream once the shim is in (net/sb-gray-shim.lisp), and the
;;;; subtype predicates used to ask STREAMP and then take the CAR of what they were given -- so
;;;; (typep a-gray-stream 'file-stream) signalled "not of type LIST", and so did glass's RFB Raw
;;;; fast path (%FS-WRITE-PIXEL-ROWS) on a WebSocket stream.  Exits 0 only if every check holds.
(when (fboundp (find-symbol "%ENSURE-GRAY-STREAMS" "CL-USER")) (funcall (find-symbol "%ENSURE-GRAY-STREAMS" "CL-USER")))
(unless (find-package "SB-GRAY") (require :sb-gray))
(defclass gst (sb-gray:fundamental-binary-input-stream sb-gray:fundamental-binary-output-stream) ())
(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (incf *pass*) (progn (incf *fail*) (format t "~&FAIL ~a: ~s, not ~s~%" name got want))))
(let ((g (make-instance 'gst)))
  (check "streamp" (and (streamp g) t) t)
  (check "typep stream" (typep g 'stream) t)
  (dolist (ty '(file-stream string-stream broadcast-stream concatenated-stream echo-stream
                synonym-stream two-way-stream))
    (check (format nil "typep ~(~a~)" ty) (handler-case (typep g ty) (error (e) (princ-to-string e))) nil))
  (check "native: string-stream" (typep (make-string-input-stream "x") 'string-stream) t)
  (check "native: broadcast-stream" (typep (make-broadcast-stream) 'broadcast-stream) t)
  (check "native: file-stream" (with-open-file (f (or *load-truename* *load-pathname*)) (typep f 'file-stream)) t))
(format t "~&gray-stream-types: ~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
