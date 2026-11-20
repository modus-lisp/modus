;;;; test/fresh-line-closed-pipe.lisp -- FRESH-LINE knows where a console, file or socket stands,
;;;; and a write to a pipe nobody reads is an error, not a silent loss.
;;;;
;;;;   MODUS_SELF=./modus ./modus --script test/fresh-line-closed-pipe.lisp   (FRESH-LINE-CLOSED-PIPE PASS)
;;;;
;;;; MODUS_SELF names this image, which the stdout check runs on a small script (modus has no
;;;; SB-EXT:*POSIX-ARGV* to find itself by).  Unset, that check is skipped and says so.
;;;;
;;;; Before: FRESH-LINE on stdout (a serial stream) and on every file stream wrote a newline every
;;;; time -- %STREAM-AT-BOL-P knew only string streams -- so text after a TERPRI gained a blank
;;;; line; WRITE-STRING/WRITE-LINE to stdout sent raw character codes, not UTF-8; and write(2)
;;;; failing on a buffered or unbuffered file stream was ignored, so writing to a child that had
;;;; exited returned normally.

#+sbcl (require :sb-bsd-sockets)
(defvar *fails* 0)
(defun check (name ok &optional detail)
  (format *error-output* "~&  ~:[FAIL~;ok  ~] ~a~@[  (~a)~]~%" ok name detail)
  (unless ok (incf *fails*)))

(defun file-text (path)
  (with-open-file (i path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8))))
      (read-sequence v i)
      (map 'string #'code-char v))))

(defparameter *dir* (format nil "/tmp/modus-fl-~d/" (random 1000000 (make-random-state t))))
(ensure-directories-exist *dir*)

;;; ---- files ----------------------------------------------------------------------------------
(let ((p (concatenate 'string *dir* "a.txt")))
  (with-open-file (o p :direction :output :if-exists :supersede)
    (check "FRESH-LINE on a new file writes nothing and says so" (null (fresh-line o)))
    (write-string "x" o)
    (check "FRESH-LINE after text writes a newline and says so" (fresh-line o))
    (check "...and a second FRESH-LINE writes nothing" (null (fresh-line o)))
    (write-string "y" o) (terpri o)
    (check "FRESH-LINE after TERPRI writes nothing" (null (fresh-line o)))
    (write-line "z" o)
    (fresh-line o)
    (write-sequence "w" o) (fresh-line o))
  (let ((got (file-text p)))
    (check "the file holds exactly x, y, z, w on four lines" (string= got (format nil "x~%y~%z~%w~%"))
           (prin1-to-string got))))

;;; ---- a synonym stream onto a file -------------------------------------------------------------
(defvar *syn-target*)
(let ((p (concatenate 'string *dir* "b.txt")))
  (with-open-file (*syn-target* p :direction :output :if-exists :supersede)
    (let ((s (make-synonym-stream '*syn-target*)))
      (write-string "a" s) (fresh-line s) (fresh-line s) (terpri s) (fresh-line s)))
  (check "a synonym stream asks the stream it names" (string= (file-text p) (format nil "a~%~%"))
         (prin1-to-string (file-text p))))

;;; ---- stdout (a serial stream on a hosted image), through a child ------------------------------
;;; Run this same image on a small script and look at its bytes, so the check sees what a pipe does.
(let* ((script (concatenate 'string *dir* "out.lisp"))
       (outp (concatenate 'string *dir* "out.txt")))
  (with-open-file (o script :direction :output :if-exists :supersede)
    (write-string "(write-string \"abc\") (fresh-line) (fresh-line) (write-string \"d\") (terpri) (fresh-line)
(write-line \"naïve €\") (fresh-line)
(sb-thread:join-thread (sb-thread:make-thread (lambda () (write-string \"w\") (fresh-line) (fresh-line))))
(fresh-line) (write-line \"end\")" o))
  (let ((self (sb-ext:posix-getenv "MODUS_SELF")))
   (if (null self)
    (format *error-output* "~&  SKIP stdout (set MODUS_SELF to this image)~%")
    (progn
    (ignore-errors
     (sb-ext:run-program self (list "--script" script) :search t :output outp :if-output-exists :supersede
                         :error nil))
    (let ((got (ignore-errors (file-text outp)))
          (want (map 'string #'code-char
                     (concatenate 'list (map 'list #'char-code (format nil "abc~%d~%"))
                                  '(110 97 195 175 118 101 32 226 130 172 10)       ; naïve € as UTF-8
                                  (map 'list #'char-code (format nil "w~%end~%"))))))
      (check "stdout: FRESH-LINE writes only where needed, WRITE-LINE is UTF-8, a worker shares the record"
             (equal got want) (prin1-to-string got)))))))

;;; ---- a pipe whose reader has exited ------------------------------------------------------------
(let* ((p (sb-ext:run-program "/bin/true" nil :input :stream :output nil :wait nil))
       (in (sb-ext:process-input p)))
  (sb-ext:process-wait p)
  (let ((c (handler-case
               (progn (dotimes (i 20000) (write-string "lost bytes " in)) (finish-output in) :no-error)
             (stream-error (e) e)
             (error (e) e))))
    (check "writing to a pipe whose reader exited signals a STREAM-ERROR" (typep c 'stream-error)
           (if (typep c 'condition) (princ-to-string c) c))
    (check "...that says why" (and (typep c 'condition) (search "Broken pipe" (princ-to-string c)))))
  (ignore-errors (close in :abort t)))

;;; ---- a socket stream: FRESH-LINE knows, as the card's control socket needs ---------------------
(let ((ok nil) (detail nil))
  (handler-case
      (let* ((srv (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        (setf (sb-bsd-sockets:sockopt-reuse-address srv) t)
        (sb-bsd-sockets:socket-bind srv #(127 0 0 1) 0)
        (sb-bsd-sockets:socket-listen srv 1)
        (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name srv)))
               (cli (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
          (sb-bsd-sockets:socket-connect cli #(127 0 0 1) port)
          (let* ((acc (sb-bsd-sockets:socket-accept srv))
                 (w (sb-bsd-sockets:socket-make-stream acc :output t :input t :element-type 'character :buffering :full))
                 (r (sb-bsd-sockets:socket-make-stream cli :input t :output t :element-type 'character :buffering :full)))
            (write-string "one" w) (fresh-line w) (fresh-line w) (write-string "two" w) (terpri w) (fresh-line w)
            (finish-output w) (close w)
            (let ((got (with-output-to-string (o) (loop for ch = (read-char r nil) while ch do (write-char ch o)))))
              (setf ok (string= got (format nil "one~%two~%")) detail (prin1-to-string got))))))
    (error (e) (setf detail (princ-to-string e))))
  (check "a socket stream: FRESH-LINE writes only where needed" ok detail))

(format *error-output* "~&FRESH-LINE-CLOSED-PIPE ~:[FAIL (~d)~;PASS~]~%" (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
