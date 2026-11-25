;;;; test/fault-note.lisp — MODUS_FAULT_NOTE=1 names the frames of a fault a HANDLER-CASE
;;;; caught; unset, a caught fault says nothing.  Run twice:
;;;;   MODUS_FAULT_NOTE=1 ./modus --load test/fault-note.lisp
;;;;   ./modus --load test/fault-note.lisp
;;;; Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

;; A read of address 16: an unmapped page, so a real SIGSEGV.
(defun fault-note-victim (a) (modus-int::%bt-word a))
(defun fault-note-caller () (fault-note-victim 16))

(let* ((wanted (sb-ext:posix-getenv "MODUS_FAULT_NOTE"))
       (said (with-output-to-string (*error-output*)
               (check "the fault is still caught"
                      (eq (handler-case (fault-note-caller) (serious-condition () :caught)) :caught)))))
  (if wanted
      (progn
        (check "and the note names the condition" (search "MODUS_FAULT_NOTE: a HANDLER-CASE caught" said))
        (check "and the frames, by name" (search "FAULT-NOTE-CALLER" said)))
      (check "unset, a caught fault says nothing" (zerop (length said)))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
