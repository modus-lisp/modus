;;;; build-linux-stdlib-test.lisp - Test stdlib.lisp in a Linux MVM binary
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(modus.mvm.x64:install-x64-translator)
(setf modus.mvm.x64::*x64-linux-mode* t)
(load (merge-pathnames "../boot/boot-linux-x64.lisp"
                       (directory-namestring (truename *load-truename*))))
(in-package :modus.mvm)

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(let* ((stdlib (read-file-text
                (merge-pathnames "../lib/stdlib.lisp"
                                 (directory-namestring (truename *load-truename*)))))
       (app "
(defun kernel-main ()
  ;; Test list ops from stdlib
  (let ((ls (list3 10 20 30)))
    (let ((rev (nreverse ls)))
      (print-dec (nth-elem 0 rev)) (space)
      (print-dec (nth-elem 1 rev)) (space)
      (print-dec (nth-elem 2 rev)) (newline)))

  ;; Test hash table from stdlib
  (let ((ht (ht-make 32)))
    (ht-put ht 1 111)
    (ht-put ht 2 222)
    (ht-put ht 3 333)
    (print-dec (ht-get ht 1)) (space)
    (print-dec (ht-get ht 2)) (space)
    (print-dec (ht-get ht 3)) (space)
    (print-dec (ht-get ht 99)) (newline))

  ;; Test assoc from stdlib
  (let ((al (list3 (cons 42 100) (cons 7 200) (cons 99 300))))
    (let ((found (assoc-eq 7 al)))
      (print-dec (cdr found)) (newline)))

  (sys-exit 0))
")
       (source (concatenate 'string stdlib app))
       (image (build-image :target :linux-x64 :source-text source)))
  (let ((path "/tmp/modus-stdlib-test"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) path)))
