;;; Round-trip of crypto/snp-guest.lisp against test/snp/fake-psp.py.
;;; Args via files in tmp/snp/: vmpck.hex, rd.hex; writes req.bin, reads resp.bin.
(defun %rd-hex-file (p) (with-open-file (s p) (let ((h (read-line s))) (let ((v (make-array (floor (length h) 2) :element-type '(unsigned-byte 8)))) (dotimes (i (length v) v) (setf (aref v i) (parse-integer h :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))))
(defun %wr-bin (p v) (with-open-file (s p :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede) (write-sequence v s)))
(defun %rd-bin (p) (with-open-file (s p :element-type '(unsigned-byte 8)) (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))
(let* ((vmpck (%rd-hex-file "tmp/snp/vmpck.hex")) (rd (%rd-hex-file "tmp/snp/rd.hex")) (seqno 41))
  (%wr-bin "tmp/snp/req.bin" (snp-report-request-page seqno rd vmpck))
  (format t "REQ-WRITTEN~%") (finish-output)
  (when (probe-file "tmp/snp/resp.bin")
    (multiple-value-bind (report status why) (snp-report-open (%rd-bin "tmp/snp/resp.bin") seqno vmpck)
      (cond ((null report) (format t "OPEN-FAILED status=~A why=~A~%" status why))
            (t (format t "REPORT ok: version ~D vmpl ~D~%report_data ~A~%measurement ~A~%RD-MATCH ~A~%"
                       (snp-report-version report) (snp-report-vmpl report) (snp-hex (snp-report-report-data report))
                       (snp-hex (snp-report-measurement report)) (if (equalp (snp-report-report-data report) rd) "YES" "NO")))))))
