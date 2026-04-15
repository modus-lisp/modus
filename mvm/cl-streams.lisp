;;;; cl-streams.lisp — Stream type system
;;;; Part of the Modus CL runtime.

;;; ============================================================
;;; String stream / printer support
;;; ============================================================

;;; ============================================================
;;; Stream type system
;;; ============================================================
;;;
;;; Stream = (cons 7770001 (cons type data))
;;; Types: 1=string-input, 2=string-output, 3=echo, 4=two-way,
;;;        5=broadcast, 6=concatenated, 7=synonym, 8=serial-io
;;;
;;; String-input data:  (cons string (cons position unread-char-or-nil))
;;; String-output data: (cons char-list nil)
;;; Echo data:          (cons input-stream output-stream)
;;; Two-way data:       (cons input-stream output-stream)
;;; Broadcast data:     list-of-streams
;;; Concatenated data:  (cons stream-list nil)
;;; Synonym data:       symbol-name-hash
;;; Serial-io data:     nil (for bare-metal serial)

(defun %stream-tag () 7770001)

(defun %make-stream (type data)
  (cons 7770001 (cons type data)))

(defun streamp (obj)
  (if (consp obj)
      (if (eq (car obj) 7770001) t nil)
      nil))

(defun %stream-type (s)
  (car (cdr s)))

(defun %stream-data (s)
  (cdr (cdr s)))

(defun input-stream-p (s)
  (if (streamp s)
      (let ((ty (%stream-type s)))
        (cond
          ((= ty 1) t)   ;; string-input
          ((= ty 3) t)   ;; echo (input+output)
          ((= ty 4) t)   ;; two-way (input+output)
          ((= ty 6) t)   ;; concatenated (input)
          ((= ty 7) t)   ;; synonym (depends on target, assume both)
          ((= ty 8) t)   ;; serial-io (both)
          ((= ty 9) t)   ;; file stream (both)
          (t nil)))
      nil))

(defun output-stream-p (s)
  (if (streamp s)
      (let ((ty (%stream-type s)))
        (cond
          ((= ty 2) t)   ;; string-output
          ((= ty 3) t)   ;; echo (input+output)
          ((= ty 4) t)   ;; two-way (input+output)
          ((= ty 5) t)   ;; broadcast (output)
          ((= ty 7) t)   ;; synonym
          ((= ty 8) t)   ;; serial-io (both)
          ((= ty 9) t)   ;; file stream (both)
          (t nil)))
      nil))

(defun open-stream-p (s)
  (if (streamp s)
      (if (= (%stream-type s) 9)
          ;; file stream: open only if fd >= 0
          (if (>= (%fs-fd s) 0) t nil)
          t)
      nil))
(defun stream-element-type (s) (quote character))
(defun stream-external-format (s) (quote default))
(defun interactive-stream-p (s) nil)

(defun close (stream &rest args) t)

;;; --- Standard stream variables ---
;;; These are defvar'd as nil above (before stream system exists).
;;; %init-streams creates actual stream objects and sets them.

(defvar *error-output* nil)
(defvar *debug-io* nil)
(defvar *query-io* nil)
(defvar *trace-output* nil)

(defun %init-streams ()
  "Initialize standard stream variables to serial-io stream objects."
  (let ((serial-in (%make-stream 8 nil))
        (serial-out (%make-stream 8 nil)))
    (setq *standard-input* serial-in)
    (setq *standard-output* serial-out)
    (setq *terminal-io* (%make-stream 4 (cons serial-in serial-out)))
    (setq *error-output* serial-out)
    (setq *debug-io* (%make-stream 4 (cons serial-in serial-out)))
    (setq *query-io* (%make-stream 4 (cons serial-in serial-out)))
    (setq *trace-output* serial-out)))

;;; --- Stream constructors ---

(defun make-string-output-stream ()
  (%make-stream 2 (cons nil nil)))

(defun get-output-stream-string (stream)
  (let ((data (if (streamp stream) (%stream-data stream) stream)))
    (let ((chars (nreverse (car data))))
      (set-car data nil)
      (let ((len (list-length chars))
            (i 0))
        (if (null chars) (%make-string-array 0)
          (let ((s (%make-string-array len))
                (cur chars))
            (loop
              (when (null cur) (return s))
              (aset s i (car cur))
              (setq cur (cdr cur))
              (setq i (+ i 1)))))))))

(defun make-string-input-stream (str &rest args)
  (let ((start (if args (car args) 0))
        (end (if (cdr args) (cadr args) nil)))
    (let ((actual-str (if (or (> start 0) end)
                          (%substring str start (if end end (length str)))
                          str)))
      (%make-stream 1 (cons actual-str (cons 0 nil))))))

(defun make-echo-stream (in out)
  (%make-stream 3 (cons in out)))

(defun make-two-way-stream (in out)
  (%make-stream 4 (cons in out)))

(defun make-broadcast-stream (&rest streams)
  (%make-stream 5 streams))

(defun make-concatenated-stream (&rest streams)
  (%make-stream 6 (cons streams nil)))

(defun make-synonym-stream (sym)
  (%make-stream 7 sym))

