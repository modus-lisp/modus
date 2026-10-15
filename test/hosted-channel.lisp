;;;; test/hosted-channel.lisp -- MODUS:MAKE-CHANNEL, messages between threads
;;;; that do not leak.
;;;;
;;;;   ./modus --script test/hosted-channel.lisp
;;;;
;;;; The property that matters is the one MODUS:SHARE does not have: passing a
;;;; message costs the runtime lock arena -- reclaimed only by a stop-the-world
;;;; collection -- NOTHING.  Section 4 measures it, then measures MODUS:SHARE on
;;;; the same meter, so the meter is known to move.  The channel goes FIRST: a
;;;; collection that rewinds the arena inside the window would hide growth.  The rest is correctness: every sendable type round-trips, refusals
;;;; happen before the channel is touched, timeouts are real, and a small ring
;;;; under back-pressure and wrap-around, a request/reply pair and two concurrent
;;;; senders all deliver exactly what was sent.

(defvar *checks* 0)
(defvar *fails* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (unless (equalp got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a~%   got  ~s~%   want ~s~%" name got want)))

(defun now-ms () (/ (get-internal-real-time) (/ internal-time-units-per-second 1000)))

;;; Threads up first, so every channel below lives in a threaded image.
(sb-thread:join-thread (sb-thread:make-thread (lambda () 0)))

;;; 1. every sendable type, through a channel on one thread
(defvar *c* (modus:make-channel))
(defun rt (x) (modus:channel-send *c* x) (modus:channel-receive *c* :timeout 1))
(defun refused (x) (handler-case (progn (modus:channel-send *c* x) :sent) (error () :refused)))

(defvar *gensym* (make-symbol "LONELY"))
(dolist (v (list nil t 0 1 -1 4611686018427387903 -4611686018427387903
                 (expt 2 200) (- (expt 3 150)) 2/3 -7/11
                 1.5d0 -0.0d0 4.9d-324 most-positive-double-float 1.5f0 -2.25f0
                 #c(1 2) #c(1.5d0 -2d0)
                 #\a #\Space (code-char 955) (code-char #x1F600)
                 "" "hello" (coerce (list #\a (code-char 955) (code-char #x1F600) #\z) 'string)
                 'car :keyword 'cl-user::chk
                 (list 1 2 3) (cons 1 2) (list 1 (list 2 (list 3 "x")) :k)
                 (vector 1 "two" (list 3))
                 (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(0 128 255))))
  (chk (format nil "round-trip ~s" v) (rt v) v))
(chk "a double's bits survive (-0.0)" (eql (rt -0.0d0) -0.0d0) t)
(chk "a symbol arrives as the SAME interned symbol" (eq (rt 'cl-user::chk) 'cl-user::chk) t)
(chk "a keyword likewise" (eq (rt :keyword) :keyword) t)
(let ((g (rt *gensym*)))
  (chk "an uninterned symbol arrives uninterned, by name"
       (list (symbol-name g) (symbol-package g) (eq g *gensym*)) (list "LONELY" nil nil)))
(let ((s "copy me"))
  (chk "a value arrives EQUAL, not EQ" (let ((r (rt s))) (list (equal r s) (eq r s))) (list t nil)))
(chk "u8 vector keeps its type" (type-of (rt (make-array 2 :element-type '(unsigned-byte 8))))
     (type-of (make-array 2 :element-type '(unsigned-byte 8))))
(let ((h (make-hash-table :test 'equal)))
  (setf (gethash "a" h) 1 (gethash (list 1 2) h) "b")
  (let ((r (rt h)))
    (chk "hash table" (list (hash-table-count r) (gethash "a" r) (gethash (list 1 2) r))
         (list 2 1 "b"))))
(chk "multiple values" (multiple-value-list (rt 42)) (list 42 t))

;;; structs: rebuilt as the same type, slots copied
(defstruct pt x y (tags nil))
(defstruct (pt3 (:include pt)) z)
(let ((r (rt (make-pt :x 1 :y "two" :tags (list :a 2)))))
  (chk "a struct arrives as its type" (pt-p r) t)
  (chk "with its slots" (list (pt-x r) (pt-y r) (pt-tags r)) (list 1 "two" (list :a 2))))
(let ((r (rt (list 7 (make-pt3 :x 1 :z 3)))))
  (chk "an included struct inside a list" (list (car r) (pt3-p (cadr r)) (pt-p (cadr r)) (pt3-z (cadr r)) (pt-x (cadr r)))
       (list 7 t t 3 1)))

;;; 2. refusals: before the channel is touched
(defclass klass () ((a :initform 1)))
(chk "a CLOS instance is refused" (refused (make-instance 'klass)) :refused)
(chk "a condition is refused" (refused (make-condition 'simple-error :format-control "x")) :refused)
(chk "a closure is refused" (refused (let ((k 1)) (lambda () k))) :refused)
(let ((l (list 1 2 3))) (setf (cdr (last l)) l)
  (chk "a circular list is refused" (refused l) :refused))
(chk "a too-big message is refused"
     (handler-case (progn (modus:channel-send (modus:make-channel :capacity 64) (make-string 100)) :sent)
       (error () :refused))
     :refused)
(chk "and nothing was left in the channel" (multiple-value-list (modus:channel-try-receive *c*))
     (list nil nil))

;;; 3. timeouts
(let ((t0 (now-ms)))
  (chk "receive on an empty channel times out" (multiple-value-list (modus:channel-receive *c* :timeout 1/5))
       (list nil nil))
  (chk "after the timeout, not before" (>= (- (now-ms) t0) 150) t))
(let ((tiny (modus:make-channel :capacity 64)))
  (modus:channel-send tiny (make-string 40))
  (chk "send to a full channel times out" (modus:channel-send tiny (make-string 40) :timeout 1/10) nil))

;;; 4. worker -> main, 20000 one-kilobyte messages, and the arena.
(defvar *big* (modus:make-channel))
(defun share-burn (n)
  (sb-thread:join-thread
   (sb-thread:make-thread
    (lambda () (dotimes (i n) (modus:share (make-string 1000 :initial-element #\s))) 0))))
(defvar *go* (modus:make-channel))
(defun send-many (ch n)
  ;; the worker starts, then waits for :GO, so the arena is read after
  ;; MAKE-THREAD's own bookkeeping
  (sb-thread:make-thread
   (lambda ()
     (modus:channel-receive *go*)
     (dotimes (i n)
       (modus:channel-send ch (list i (make-string 1000 :initial-element (code-char (+ 65 (mod i 26)))))))
     0)))
(let* ((n 20000) (th (send-many *big* n)) (a0 (%rt-arena-alloc)) (bad 0) (t0 (now-ms)))
  (modus:channel-send *go* :go)
  (dotimes (i n)
    (let ((m (modus:channel-receive *big* :timeout 30)))
      (unless (and (consp m) (eql (car m) i) (= (length (cadr m)) 1000)
                   (char= (char (cadr m) 999) (code-char (+ 65 (mod i 26)))))
        (setq bad (+ bad 1)))))
  (sb-thread:join-thread th)
  (format t "~&  20000 x 1 KB worker -> main in ~,1f s; arena moved ~a bytes~%"
          (/ (- (now-ms) t0) 1000.0) (- (%rt-arena-alloc) a0))
  (chk "20000 messages, every one intact and in order" bad 0)
  (chk "and the arena did not move" (- (%rt-arena-alloc) a0) 0))

;; the positive control: MODUS:SHARE moves the meter
(let ((a0 (%rt-arena-alloc)))
  (share-burn 300)
  (chk "control: 300 MODUS:SHARE copies use the arena" (> (- (%rt-arena-alloc) a0) 1000000) t))

;;; 5. back-pressure and wrap-around: a 4 KB ring, 5000 messages of ~300 bytes
(defvar *small* (modus:make-channel :capacity 4096))
(let* ((n 5000)
       (th (sb-thread:make-thread
            (lambda ()
              (dotimes (i n) (modus:channel-send *small* (list i (make-string (+ 200 (mod i 150)) :initial-element #\w))))
              0)))
       (bad 0))
  (dotimes (i n)
    (let ((m (modus:channel-receive *small* :timeout 30)))
      (unless (and (consp m) (eql (car m) i) (= (length (cadr m)) (+ 200 (mod i 150))))
        (setq bad (+ bad 1)))))
  (sb-thread:join-thread th)
  (chk "a 4 KB ring carries 5000 messages through wrap-around" bad 0))

;;; 6. request / reply
(defvar *req* (modus:make-channel))
(defvar *rep* (modus:make-channel))
(let ((th (sb-thread:make-thread
           (lambda ()
             (loop (let ((m (modus:channel-receive *req*)))
                     (when (eq m :stop) (return 0))
                     (modus:channel-send *rep* (list (car m) (* 2 (cadr m)) (string-upcase (caddr m))
                                                     (make-pt :x (car m))))))))))
  (let ((bad 0))
    (dotimes (i 2000)
      (modus:channel-send *req* (list i (* i i) "ping"))
      (let ((r (modus:channel-receive *rep* :timeout 10)))
        (unless (and (equal (subseq r 0 3) (list i (* 2 i i) "PING"))
                     (pt-p (fourth r)) (eql (pt-x (fourth r)) i))
          (setq bad (+ bad 1)))))
    (modus:channel-send *req* :stop)
    (chk "2000 request/reply round trips, a struct in every reply" bad 0)
    (chk "the worker stopped on :STOP" (sb-thread:join-thread th) 0)))

;;; 7. two senders, one channel: messages never interleave
(defvar *two* (modus:make-channel :capacity 8192))
(let* ((a (sb-thread:make-thread (lambda () (dotimes (i 3000) (modus:channel-send *two* (list :a i (make-string 50 :initial-element #\a)))) 0)))
       (b (sb-thread:make-thread (lambda () (dotimes (i 3000) (modus:channel-send *two* (list :b i (make-string 70 :initial-element #\b)))) 0)))
       (next-a 0) (next-b 0) (bad 0))
  (dotimes (i 6000)
    (let ((m (modus:channel-receive *two* :timeout 30)))
      (cond ((and (eq (car m) :a) (eql (cadr m) next-a) (= (length (caddr m)) 50)) (setq next-a (+ next-a 1)))
            ((and (eq (car m) :b) (eql (cadr m) next-b) (= (length (caddr m)) 70)) (setq next-b (+ next-b 1)))
            (t (setq bad (+ bad 1))))))
  (sb-thread:join-thread a) (sb-thread:join-thread b)
  (chk "two concurrent senders: every message whole, each sender in order" (list bad next-a next-b) (list 0 3000 3000)))

(format t "~&CHANNEL: ~a checks, ~a failed~%" *checks* *fails*)
(sys-exit (if (zerop *fails*) 0 1))
