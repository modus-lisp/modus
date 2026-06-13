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
          ((= ty 2) nil) ;; string-output
          ((= ty 3) t)   ;; echo (input+output)
          ((= ty 4) t)   ;; two-way (input+output)
          ((= ty 5) nil) ;; broadcast (output only)
          ((= ty 6) t)   ;; concatenated (input)
          ;; synonym: delegate to the symbol-value target stream.
          ((= ty 7)
           (let ((target (symbol-value (%stream-data s))))
             (if (streamp target) (input-stream-p target) nil)))
          ((= ty 8) t)   ;; serial-io (both)
          ((= ty 9)      ;; file stream: input or io direction
           (let ((d (%fs-dir s)))
             (if (eq d :output) nil t)))
          (t nil)))
      nil))

(defun output-stream-p (s)
  (if (streamp s)
      (let ((ty (%stream-type s)))
        (cond
          ((= ty 1) nil) ;; string-input
          ((= ty 2) t)   ;; string-output
          ((= ty 3) t)   ;; echo (input+output)
          ((= ty 4) t)   ;; two-way (input+output)
          ((= ty 5) t)   ;; broadcast (output)
          ((= ty 6) nil) ;; concatenated (input only)
          ;; synonym: delegate to the symbol-value target stream.
          ((= ty 7)
           (let ((target (symbol-value (%stream-data s))))
             (if (streamp target) (output-stream-p target) nil)))
          ((= ty 8) t)   ;; serial-io (both)
          ((= ty 9)      ;; file stream: output or io direction
           (let ((d (%fs-dir s)))
             (if (eq d :input) nil t)))
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
(defun stream-external-format (s) :default)
(defun interactive-stream-p (s) nil)

(defun close (stream &rest args) t)

;;; --- Stream subtype predicates ---
;;; Used by typep dispatch (and directly callable from CL code) to
;;; identify particular stream "shapes".  Each is parameterized on the
;;; type-id stored at (cadr stream): 1=string-input, 2=string-output,
;;; 3=echo, 4=two-way, 5=broadcast, 6=concatenated, 7=synonym,
;;; 8=serial-io, 9=file.

(defun string-stream-p (s)
  "Return t if S is a string stream (input or output)."
  (if (streamp s)
      (let ((ty (%stream-type s)))
        (if (= ty 1) t (if (= ty 2) t nil)))
      nil))

(defun broadcast-stream-p (s)
  "Return t if S is a broadcast stream."
  (if (streamp s)
      (if (= (%stream-type s) 5) t nil)
      nil))

(defun concatenated-stream-p (s)
  "Return t if S is a concatenated stream."
  (if (streamp s)
      (if (= (%stream-type s) 6) t nil)
      nil))

(defun echo-stream-p (s)
  "Return t if S is an echo stream."
  (if (streamp s)
      (if (= (%stream-type s) 3) t nil)
      nil))

(defun synonym-stream-p (s)
  "Return t if S is a synonym stream."
  (if (streamp s)
      (if (= (%stream-type s) 7) t nil)
      nil))

(defun two-way-stream-p (s)
  "Return t if S is a two-way stream."
  (if (streamp s)
      (if (= (%stream-type s) 4) t nil)
      nil))

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

(defun make-string-output-stream (&rest args)
  "Create a string-output stream.  Per CLHS, accepts &key element-type;
   we ignore the actual value (all stream output is character-based).
   Validate the kwarg shape — odd-length / unknown keys (without
   :allow-other-keys T) signal program-error per CLHS 3.4.1.4."
  (let ((allow-other nil))
    (let ((scan args))
      (loop (when (or (null scan) (null (cdr scan))) (return))
        (when (and (eq (car scan) :allow-other-keys) (cadr scan))
          (setq allow-other t))
        (setq scan (cddr scan))))
    (let ((vp args))
      (loop
        (when (null vp) (return))
        (when (null (cdr vp)) (%signal-program-error) (return))
        (let ((k (car vp)))
          (unless (or (eq k :element-type)
                      (eq k :allow-other-keys)
                      allow-other)
            (%signal-program-error)
            (return)))
        (setq vp (cddr vp)))))
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
  ;; CLHS: (make-string-input-stream string &optional start end).
  ;; A 4th positional argument is a program-error.
  (when (cddr args) (%signal-program-error))
  (let ((start (if args (car args) 0))
        (end (if (cdr args) (cadr args) nil)))
    (let ((actual-str (if (or (> start 0) end)
                          (%substring str start (if end end (length str)))
                          str)))
      (%make-stream 1 (cons actual-str (cons 0 nil))))))

(defun %string-input-stream-pos (stream)
  "Return the current read position of a string-input stream.  Used by
   WITH-INPUT-FROM-STRING's :index option (CLHS) to setf the user's
   place to the final position when the body returns normally."
  (if (streamp stream)
      (let ((data (%stream-data stream)))
        ;; data = (cons actual-str (cons pos nil)) — pos is cadr.
        (car (cdr data)))
      0))

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

