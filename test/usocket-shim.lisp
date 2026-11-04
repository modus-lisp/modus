;;;; test/usocket-shim.lisp -- stock usocket (Quicklisp's, unpatched) on modus.
;;;;
;;;; usocket has no modus backend and takes its Genera one, whose file does not read here.
;;;; net/asdf-interface.lisp masks :GENERA from usocket's components and installs a backend
;;;; over the SB-BSD-SOCKETS shim (ASDF::%QUIRK-USOCKET).  This loads usocket from source and
;;;; runs a loopback round trip.
;;;;   USOCKET_DIR=... SPLIT_SEQUENCE_DIR=... ./modus --script test/usocket-shim.lisp
;;;; (defaults: the newest of each under ~/quicklisp/dists/quicklisp/software/).  Exit 0 = pass.
(defun %newest (prefix)
  (let ((all (directory (concatenate 'string (namestring (user-homedir-pathname))
                                     "quicklisp/dists/quicklisp/software/" prefix "*/"))))
    (car (last (sort (mapcar #'namestring all) #'string<)))))
(dolist (d (list (or (modus-int::%cli-getenv "USOCKET_DIR") (%newest "usocket-"))
                 (or (modus-int::%cli-getenv "SPLIT_SEQUENCE_DIR") (%newest "split-sequence-"))))
  (unless d (format t "~&usocket-shim: SKIP (no usocket source)~%") (sb-ext:exit :code 0))
  (push (pathname (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/")))
        asdf:*central-registry*))
(asdf:load-system "usocket")
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want) (format t "ok    ~a~%" name)
      (progn (incf *fails*) (format t "FAIL  ~a: ~s want ~s~%" name got want))))
(let* ((srv (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8)))
       (port (usocket:get-local-port srv))
       (cli (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
       (acc (usocket:socket-accept srv)))
  (check "a port is assigned" (plusp port) t)
  (write-sequence #(1 2 3 4 5) (usocket:socket-stream cli))
  (force-output (usocket:socket-stream cli))
  (check "wait-for-input sees the data" (and (usocket:wait-for-input acc :timeout 2 :ready-only t) t) t)
  (let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
    (read-sequence buf (usocket:socket-stream acc))
    (check "the bytes arrive" (coerce buf 'list) '(1 2 3 4 5)))
  (check "wait-for-input times out on an idle socket"
         (usocket:wait-for-input cli :timeout 0.2 :ready-only t) nil)
  (usocket:socket-close cli) (usocket:socket-close acc) (usocket:socket-close srv))
(format t "~&usocket-shim: ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
