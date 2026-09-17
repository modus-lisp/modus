(setq *jit-hot-only* nil)
(defun words (dx dy dw dh sw sh p0 p1 p2 ys cs ks)
  (let ((ppf (lambda (s d) (logior (ash 1 30) (ash (floor (* 65536 s) d) 8)))))
    (list (logior (ash 1 30) (ash 28 24) 8) (logior #xFF000000 (ash dy 12) dx) (logior (ash dh 16) dw)
          (logior (ash 1 30) (ash sh 16) sw) #xC0C0C0C0
          (logior #xC0000000 p0) (logior #xC0000000 p1) (logior #xC0000000 p2)
          #xC0C0C0C0 #xC0C0C0C0 #xC0C0C0C0 ys cs cs #x00f00000 #xe73304a8 #x00066604 0
          (funcall ppf (ash sw -1) dw) (funcall ppf (ash sh -1) dh) #xC0C0C0C0
          (funcall ppf sw dw) (funcall ppf sh dh) #xC0C0C0C0 ks ks ks ks #x80000000)))
(modus:jit-eager)
(format t "JIT ~a~%" (words 0 60 1920 1080 320 180 340025376 340114448 340139024 384 192 2100))
(format t "EXP (1543503880 4278435840 70782720 1085538624 3233857728 3561250848 3561339920 3561364496 3233857728 3233857728 3233857728 384 192 192 15728640 3878880424 419332 0 1075139840 1075139840 3233857728 1076537856 1076537856 3233857728 2100 2100 2100 2100 2147483648)~%")
(finish-output)
