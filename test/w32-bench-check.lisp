;;;; test/w32-bench-check.lisp -- check test/w32-bench.lisp's local copies
;;;; against natrium on SBCL, and against independent vectors; then print the
;;;; SBCL throughput (the same W32B-RUN the Modus images run).
;;;;
;;;;   sbcl --script test/w32-bench-check.lisp [SHA-KB CHACHA-KB]
;;;;
;;;; natrium is found as ../natrium relative to this tree's parent (the kiln
;;;; workspace layout), or at $NATRIUM_DIR.  Exit 0 iff every check passed.

(require :asdf)
(let* ((here (directory-namestring (truename *load-truename*)))
       (env (sb-ext:posix-getenv "NATRIUM_DIR"))
       (dir (if (and env (plusp (length env)))
                (pathname (concatenate 'string (string-right-trim "/" env) "/"))
                (merge-pathnames "../../natrium/" here))))
  (push dir asdf:*central-registry*)
  (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (asdf:load-system :natrium))
  (load (merge-pathnames "w32-bench.lisp" here)))

(defvar *fail* 0)
(defun check (name got want)
  (if (equalp got want)
      (format t "PASS ~A~%" name)
      (progn (incf *fail*) (format t "FAIL ~A~%  got  ~A~%  want ~A~%" name got want))))

(defun hexv (s)
  (let ((v (make-array (/ (length s) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length v) v)
      (setf (aref v i) (parse-integer s :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

;;; Independent vectors: FIPS 180-2 "abc"; Python hashlib over the
;;; (i*7+3) mod 256 pattern (test/crypto-sha-vectors.lisp); RFC 8439 2.3.2.
(check "sha256 abc (FIPS 180-2)"
       (w32b-hex (w32b-sha256 (map '(vector (unsigned-byte 8)) #'char-code "abc")))
       "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
(check "sha256 pattern 65536 (hashlib)"
       (w32b-hex (w32b-sha256 (w32b-pattern 65536)))
       "510b126e1d4ced49107fe4ab03ee54cb1c8e4caf6064e1dd29c48d4a3e74c38b")
(check "chacha20 block (RFC 8439 2.3.2)"
       (w32b-hex (w32b-chacha20-xor (w32b-key)
                                    (let ((v (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
                                      (setf (aref v 3) #x09 (aref v 7) #x4a) v)
                                    1 (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
       (concatenate 'string
                    "10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4e"
                    "d2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e"))

;;; The copies against natrium, every padding boundary and a few sizes.
(dolist (n '(0 1 3 55 56 63 64 65 111 112 119 120 127 128 129 1000 4096 65537))
  (let ((m (w32b-pattern n)))
    (check (format nil "sha256 vs natrium n=~D" n)
           (w32b-sha256 m) (natrium:sha256 m))
    (check (format nil "chacha20 vs natrium n=~D" n)
           (w32b-chacha20-xor (w32b-key) (w32b-nonce) 1 m)
           (natrium:chacha20 (w32b-key) (w32b-nonce) m :counter 1))))
(format t "~D failure~:P~%" *fail*)
(let ((args (cdr sb-ext:*posix-argv*)))
  (when (= *fail* 0)
    (format t "--- SBCL ~A, compiled with SBCL's defaults ---~%" (lisp-implementation-version))
    (w32b-run (if args (parse-integer (first args)) 4096)
              (if args (parse-integer (second args)) 4096))))
(sb-ext:exit :code (if (= *fail* 0) 0 1))
