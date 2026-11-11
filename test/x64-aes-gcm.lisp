;;;; x64-aes-gcm.lisp -- the native AES-GCM halves (KERNEL kinds 3-5, mvm/native-pixels-x64.lisp)
;;;; against seal's Lisp: counter mode over both key sizes, edge lengths and a wrapping
;;;; counter; GHASH over 300 random inputs; bad arguments declined; xmm registers kept.
;;;; Needs seal on ASDF's registry.  Prints "native aes-gcm checks: 0 failures".
(require :asdf)
(let ((root (sb-ext:posix-getenv "MODUS_ROOT"))) (when root (dolist (d (directory (format nil "~A/*/" root))) (push d asdf:*central-registry*))))
(let ((ql (sb-ext:posix-getenv "QUICKLISP_SOFTWARE")))     ; third-party systems, behind the tree
  (when ql (dolist (d (directory (format nil "~A/*/" ql))) (setf asdf:*central-registry* (append asdf:*central-registry* (list d))))))
;; seal from the registry (MODUS_ROOT layout): it supplies the Lisp reference this checks against
(asdf:load-system :seal)
(defvar *bad* 0)
;; the Lisp-only counter mode: a seal with the runtime hook sends %GCM-CTR itself to the native
;; path, which would make this compare the kernel with itself
(defvar *ctr-reference* (if (fboundp 'seal::%gcm-ctr-octets) 'seal::%gcm-ctr-octets 'seal::%gcm-ctr))
(defun u8 (n f) (let ((v (make-array n :element-type '(unsigned-byte 8)))) (dotimes (i n v) (setf (aref v i) (funcall f i)))))
(defvar *seed* 99)
(defun rnd () (setq *seed* (mod (+ (* *seed* 1103515245) 12345) 2147483648)) (ldb (byte 8 16) *seed*))
(format t "~&cpu ok: ~a~%" (modus-int::%x64-aes-gcm-p))
;; counter mode
(dolist (klen '(16 32))
  (dolist (n '(0 1 15 16 17 31 32 33 1200 1201))
    (dolist (wrap '(nil t))
      (multiple-value-bind (ek rounds) (seal::%gcm-prepare-key (u8 klen (lambda (i) (declare (ignore i)) (rnd))))
        (let* ((j0 (u8 16 (lambda (i) (cond ((and wrap (>= i 12)) #xFF) ((= i 15) 1) (t (rnd))))))
               (pt (u8 n (lambda (i) (declare (ignore i)) (rnd))))
               (ks (concatenate '(simple-array (unsigned-byte 8) (*)) j0 ek))
               (out (make-array n :element-type '(unsigned-byte 8)))
               (ok (modus.runtime::%aes-ctr-xor ks pt out n rounds)))
          (unless (and ok (equalp out (funcall *ctr-reference* ek rounds j0 pt)))
            (incf *bad*) (format t "CTR MISMATCH klen ~a n ~a wrap ~a ok ~a~%" klen n wrap ok)))))))
;; ghash
(let ((mask (u8 16 (lambda (i) (- 15 i)))))
  (dotimes (trial 300)
    (let* ((h (u8 16 (lambda (i) (declare (ignore i)) (rnd))))
           (data (u8 (* 16 (mod trial 80)) (lambda (i) (declare (ignore i)) (rnd))))
           (state (concatenate '(simple-array (unsigned-byte 8) (*)) h (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0) mask))
           (ok (modus.runtime::%ghash-update state data (length data))))
      (unless (and ok (equalp (subseq state 16 32) (seal::%gcm-ghash-32 h data)))
        (incf *bad*) (format t "GHASH MISMATCH trial ~a len ~a~%" trial (length data))))))
;; bad arguments decline
(let ((v (u8 64 #'identity)))
  (dolist (c (list (modus.runtime::%aes-ctr-xor v v v 10 11)       ; rounds 11
                   (modus.runtime::%aes-ctr-xor (u8 100 #'identity) v v 10 10) ; ks too short
                   (modus.runtime::%aes-ctr-xor (u8 192 #'identity) v v 65 10) ; bytes > in
                   (modus.runtime::%aes-ctr-xor (list 1 2) v v 10 10)
                   (modus.runtime::%ghash-update v v 17)
                   (modus.runtime::%ghash-update (u8 40 #'identity) v 16)))
    (when c (incf *bad*) (format t "BAD ARGUMENT ACCEPTED~%"))))
;; xmm registers survive
(let* ((x 1.5d0) (y 2.25d0) (ek (seal::%gcm-prepare-key (u8 16 #'identity)))
       (ks (concatenate '(simple-array (unsigned-byte 8) (*)) (u8 16 #'identity) ek))
       (v (u8 64 #'identity)) (st (concatenate '(simple-array (unsigned-byte 8) (*)) (u8 32 #'identity) (u8 16 (lambda (i) (- 15 i))))))
  (let ((r (* x (progn (modus.runtime::%aes-ctr-xor ks v v 64 10) (modus.runtime::%ghash-update st v 64) y))))
    (unless (= r 3.375d0) (incf *bad*) (format t "FLOAT CLOBBERED ~a~%" r))))
(format t "native aes-gcm checks: ~d failures~%" *bad*)
